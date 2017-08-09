%%%-------------------------------------------------------------------
%% @doc epl_ets_viz_map module.
%% This module provides an APIs for manipulating a Vizceral data structure which
%% makes it possible to visualise ETS charcteristics.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets_viz_map).

%% API
-export([update_cluster/2,
         update_details/3,
         remove_outdated/2]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Updates Vizceral map's cluster section.
-spec update_cluster(Node :: atom(), Viz :: map()) -> map().
update_cluster(Node, Viz = #{nodes := VizNodes}) ->
    Nodes = extract_nodes_from_viz(VizNodes),
    VizRegion = push_unique_region(Node, Viz, lists:member(Node, Nodes)),
    ETSBasicInfo = get_node_ets_basic_info(Node),
    epl_viz_map:push_additional_node_info(ETSBasicInfo, Node, VizRegion).

%% @doc Updates Vizceral map's third-level section.
-spec update_details(Node :: node(), [] | list(), Viz :: map()) -> map().
update_details(Node, [], Viz) ->
    VizCleaned = clean_ets_traffic_from_viz(Node, Viz),
    push_ets_tables(undefined, Node, [], VizCleaned);
update_details(Node, ETSTrafficCounters, Viz) ->
    VizCleaned = clean_ets_traffic_from_viz(Node, Viz),
    {tab_traffic, ETSTraffic} =
        epl_ets_metric:get_ets_tab_traffic(ETSTrafficCounters),
    push_tab_traffic(ETSTraffic, Node, VizCleaned).

%% @doc Removes outdated nodes from Vizceral map's cluster section.
-spec remove_outdated(Nodes :: [#{atom() => integer()}], Viz :: map()) -> map().
remove_outdated(Nodes, Viz) ->
    NodesList = maps:keys(Nodes),
    VizNodes = [pull_node(N, Viz) || N <- NodesList],
    VizConn = [pull_fake_conn(N, Viz) || N <- NodesList],
    NewViz = maps:merge(Viz, #{nodes => VizNodes}),
    maps:merge(NewViz, #{connections => VizConn}).

%%====================================================================
%% Internals
%%====================================================================

push_tab_traffic([{Tab, Traffic}], Node, Viz) ->
    Viz2 = push_ets_tables(Tab, Node, Traffic, Viz),
    lists:foldl(fun(Proc, VizUpdated) -> push_ets_proc_and_conn(Tab, Proc, Node,
                                                                VizUpdated) end,
                Viz2, Traffic).

push_ets_tables(Tab, Node, Traffic, Viz) ->
    RestTabs = lists:delete(Tab, epl_ets_metric:get_node_ets_tabs(Node)),
    TracedTabAdditional = get_trace_tab_additional_info_map(Traffic),
    Viz2 = lists:foldl(fun(T, VizUpdated) ->
                               epl_viz_map:push_focused(T, Node,
                                                        create_ets_traffic_metrics_map(0, 0),
                                                        VizUpdated) end,
                       Viz, RestTabs),
    epl_viz_map:push_focused(Tab, Node, TracedTabAdditional, Viz2).

get_trace_tab_additional_info_map([]) ->
    create_ets_traffic_metrics_map(0, 0);
get_trace_tab_additional_info_map(Traffic) ->
    {Insert, Lookup}  = sum_tab_traffic(Traffic),
    create_ets_traffic_metrics_map(Insert, Lookup).

create_ets_traffic_metrics_map(Insert, Lookup) ->
    InsertToAll = calculate_percent(Insert, Lookup),
    #{etsMetrics => #{insert => Insert, lookup => Lookup,
                      pieChart => create_viz_node_pie_chart(InsertToAll, 0, 0)}}.

sum_tab_traffic(Traffic) ->
    lists:foldl(fun sum_tab_traffic_counters/2, {0,0}, Traffic).

sum_tab_traffic_counters({_Pid, Counters}, {InsertSum, LookupSum}) ->
    Insert = proplists:get_value(insert, Counters),
    Lookup = proplists:get_value(lookup, Counters),
    {I, L} = ensure_traffic_counters_are_num(Insert, Lookup),
    {InsertSum + I, LookupSum + L}.

ensure_traffic_counters_are_num(undefined, Lookup) ->
    {0, Lookup};
ensure_traffic_counters_are_num(Insert, undefined) ->
    {Insert, 0};
ensure_traffic_counters_are_num(undefined, undefined) ->
    {0, 0};
ensure_traffic_counters_are_num(Insert, Lookup) ->
    {Insert, Lookup}.

push_ets_proc_and_conn(Tab, {Pid, Counters}, Node, Viz) ->
    Insert = proplists:get_value(insert, Counters),
    Lookup = proplists:get_value(lookup, Counters),
    {I, L} = ensure_traffic_counters_are_num(Insert, Lookup),
    Viz2 = epl_viz_map:push_focused(Pid, Node,
                                    create_ets_traffic_metrics_map(I, L), Viz),
    Viz3 = epl_viz_map:push_focused_connection(Pid, Tab, Node, {I, 0, 0}, Viz2),
    epl_viz_map:push_focused_connection(Tab, Pid, Node, {L, 0, 0}, Viz3).

clean_ets_traffic_from_viz(Node, Viz) ->
    {VizNode, NewViz = #{nodes := VizNodes}} = epl_viz_map:pull_region(Node,
                                                                       Viz),
    VizNodeCleaned = maps:merge(VizNode, #{nodes => []}),
    VizNodeCleaned2 = maps:merge(VizNodeCleaned, #{connections => []}),
    maps:merge(NewViz, #{nodes => [VizNodeCleaned2 | VizNodes]}).

get_node_ets_basic_info(Node) ->
    ETSCount = epl_ets_metric:get_node_ets_num(Node),
    ETSMemUsage = epl_ets_metric:get_node_ets_mem(Node),
    ETSPieChart = create_viz_node_pie_chart(ETSMemUsage, 0, 0),
    create_node_ets_viz_metric_map(ETSCount, ETSMemUsage, ETSPieChart).

create_viz_node_pie_chart(N, D, W) ->
    #{normal => N, danger => D, warning => W}.

create_node_ets_viz_metric_map(ETSCount, ETSMemUsage, ETSPieChart) ->
    #{etsMetrics => 
          #{all => ETSCount, memUsage => ETSMemUsage, pieChart => ETSPieChart}}.

push_unique_region(Node, Viz, false) ->
    VizRegion = epl_viz_map:push_region(Node, Viz),
    VizRegion2 = push_fake_conn(Node, VizRegion),
    epl_viz_map:push_focused(fake, Node, VizRegion2);
push_unique_region(_Node, Viz, true) ->
    Viz.

push_fake_conn(Node, Viz) ->
    epl_viz_map:push_connection(epl:get_default_node(), Node, {0, 0, 0}, #{}, 
                                Viz).

pull_node(Node, Viz) ->
    {N, _Rest} = epl_viz_map:pull_node(Node, Viz),
    N.

pull_fake_conn(Node, #{connections := Conns}) ->
    [Conn] = lists:filter(fun(Elem) -> maps:get(target, Elem) == 
                                           epl_viz_map:namify(Node) end, Conns),
    Conn.

extract_nodes_from_viz(Nodes) ->
    lists:map(fun(N) -> 
                      Name = maps:get(displayName, N),
                      erlang:binary_to_atom(Name, latin1)
              end, Nodes).

calculate_percent(0, 0) ->
    0;
calculate_percent(A, B) ->
    epl_ets_metric:trunc_float(A / (A + B), 2).

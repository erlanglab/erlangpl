%%%-------------------------------------------------------------------
%% @doc epl_ets_viz_map module.
%% This module provides an APIs for manipulating a Vizceral data structure which
%% makes it possible to visualise ETS charcteristics.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets_viz_map).

%% API
-export([update_cluster/2,
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

%% ETS cluster view --------------------------------------------------
get_node_ets_basic_info(Node) ->
    ETSCount = epl_ets_metric:get_node_ets_num(Node),
    ETSMemUsage = epl_ets_metric:get_node_ets_mem(Node),
    ETSPieChart = create_node_ets_pie_chart(ETSMemUsage, 0, 0),
    create_node_ets_viz_metric_map(ETSCount, ETSMemUsage, ETSPieChart).

create_node_ets_pie_chart(N, D, W) ->
    #{normal => N, danger => D, warning => W}.

create_node_ets_viz_metric_map(ETSCount, ETSMemUsage, ETSPieChart) ->
    #{etsMetrics => 
          #{all => ETSCount, memUsage => ETSMemUsage, pieChart => ETSPieChart}}.

push_unique_region(Node, Viz, false) ->
    VizRegion = epl_viz_map:push_region(Node, Viz),
    push_fake_conn(Node, VizRegion);
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

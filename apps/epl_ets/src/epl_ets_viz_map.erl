%%%-------------------------------------------------------------------
%% @doc epl_ets_viz_map module.
%% This module provides an APIs for manipulating a Vizceral data structure which
%% makes it possible to visualise ETS charcteristics.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets_viz_map).

%% API
-export([update_cluster/2,
         update_node/3,
         remove_outdated/2]).

%% Consts
-define(ETS_MEM_WARN, 5).
-define(ETS_MEM_DANGER, ?ETS_MEM_WARN).
-define(ETS_SIZE_WARN, ?ETS_MEM_WARN).
-define(ETS_SIZE_DANGER, ?ETS_MEM_WARN).

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

%% @doc Updates  the Vizceral map's node section.
-spec update_node(Node :: atom(), Viz :: map(), Mode :: atom()) -> map().
update_node(Node, Viz, Mode) ->
    VizCleaned = clean_ets_from_viz(Node, Viz),
    ETSTabs = epl_ets_metric:get_node_ets_tabs(Node),
    ETSTabsEndPoint = epl_ets_metric:get_ets_tabs_owner(Node, ETSTabs),
    ETSTabsMetric = get_ets_metric(Node, ETSTabs, Mode),
    push_ets_tables(Node, ETSTabs, ETSTabsEndPoint, ETSTabsMetric, VizCleaned).

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

%% ETS node view ------------------------------------------------------
clean_ets_from_viz(Node, Viz) ->
    {VizNode, NewViz = #{nodes := VizNodes}} = epl_viz_map:pull_region(Node, 
                                                                       Viz),
    VizNodeCleaned = maps:merge(VizNode, #{nodes => []}),
    VizNodeCleaned2 = maps:merge(VizNodeCleaned, #{connections => []}),
    maps:merge(NewViz, #{nodes => [VizNodeCleaned2 | VizNodes]}).

get_ets_metric(Node, ETSTabs, memory) ->
    Tabs = epl_ets_metric:get_ets_tabs_mem_sorted(Node, ETSTabs),
    TabsSplited = split_list_viz_severity(Tabs, ?ETS_MEM_DANGER, ?ETS_MEM_WARN),
    NoticeFun = fun(Metric, Severity) -> create_ets_mem_viz_notice(Metric, 
                                                                   Severity) end,
    add_notice_and_merge(TabsSplited, NoticeFun);
get_ets_metric(Node, ETSTabs, size) ->
    Tabs = epl_ets_metric:get_ets_tabs_size_sorted(Node, ETSTabs),
    TabsSplited = split_list_viz_severity(Tabs, ?ETS_SIZE_DANGER, 
                                          ?ETS_SIZE_WARN),
    NoticeFun = fun(Metric, Severity) -> create_ets_size_viz_notice(Metric,
                                                                    Severity) end,
    add_notice_and_merge(TabsSplited, NoticeFun).


push_ets_tables(Node, Tabs, TabsEP, TabsMetric, Viz) ->
    lists:foldl(fun(Tab, VizUpdated) -> push_ets_and_conn(Node, Tab, TabsEP,
                                                    TabsMetric, VizUpdated) end,
                Viz, Tabs).

push_ets_and_conn(Node, Tab, TabsEP, TabsMetric, Viz) ->
    TabEP = proplists:get_value(Tab, TabsEP),
    NewViz = epl_viz_map:push_focused(TabEP, Node, Viz),
    {TabClass, TabNotices} = proplists:get_value(Tab, TabsMetric),
    TabMetricVizMap = create_ets_viz_metric_map(TabClass, [TabNotices]),
    NewViz2 = epl_viz_map:push_focused(Tab, Node, TabMetricVizMap, NewViz),
    epl_viz_map:push_focused_connection(Tab, TabEP, Node, {0, 0, 0},
                                        NewViz2).

add_notice_and_merge(TabsSplited, NF) ->
    lists:merge([lists:map(fun(T) -> create_ets_viz_metric(T, S, NF) end, Tab)
                 || {S, Tab} <- TabsSplited]).

create_ets_viz_metric({Tab, M}, 2, NF) ->
    {Tab, {<<"danger">>, NF(M, 2)}};
create_ets_viz_metric({Tab, M}, 1, NF) ->
    {Tab, {<<"warning">>, NF(M, 1)}};
create_ets_viz_metric({Tab, M}, 0, NF) ->
    {Tab, {<<"normal">>, NF(M, 0)}}.

create_ets_mem_viz_notice(Mem, Severity) ->
    MemBytes = erlang:system_info(wordsize) * Mem,
    #{title => concat_binary(<<"Used memory in bytes: ">>, 
                             erlang:integer_to_binary(MemBytes)),
      severity => Severity}.

create_ets_size_viz_notice(Size, Severity) ->
    #{title => concat_binary(<<"Number of elements is: ">>, 
                             erlang:integer_to_binary(Size)),
      severity => Severity}.

split_list_viz_severity(List, S1Item, S2Item) ->
    {List2, Rest} = lists:split(S1Item, List),
    {List1, List0} = lists:split(S2Item, Rest),
    [{2, List2}, {1, List1}, {0, List0}].

%% Common functions ---------------------------------------------------
concat_binary(A, B) ->
    <<A/binary, B/binary>>.

create_ets_viz_metric_map(Class, Notices) ->
    #{class => Class, notices => Notices}.


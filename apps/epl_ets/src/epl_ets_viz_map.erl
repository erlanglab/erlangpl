%%%-------------------------------------------------------------------
%% @doc epl_ets_viz_map module.
%% This module provides API for building a Vizceral data structure which
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
    ETSBasicInfo = get_ets_basic_info(Node),
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

get_ets_basic_info(Node) ->
    ETSCount = get_all_ets_count(Node),
    ETSMemUsage = get_ets_mem_usage(Node),
    ETSPieChart = get_ets_pie_chart(ETSMemUsage, 0, 0),
    #{etsMetrics => 
          #{all => ETSCount, memUsage => ETSMemUsage, pieChart => ETSPieChart}}.

get_all_ets_count(Node) ->
    {ok, AllETS} = epl:command(Node, fun ets:all/0, []),
    erlang:length(AllETS).

get_ets_mem_usage(Node) ->
    {ok, MemoryData} = epl:command(Node, fun erlang:memory/0, []),
    MemoryPercent = proplists:get_value(ets, MemoryData) / 
        proplists:get_value(total, MemoryData),
    trunc_float(MemoryPercent, 4).

get_ets_pie_chart(N, D, W) ->
    #{normal => N, danger => D, warning => W}.

trunc_float(Float, Pos) ->
    List = erlang:float_to_list(Float, [{decimals, Pos}]),
    erlang:list_to_float(List).

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

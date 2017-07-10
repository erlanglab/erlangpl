%%%-------------------------------------------------------------------
%% @doc epl_ets_tab_map module.
%% This module provides an APIs for manipulating a data structure which is
%% showed in a table in the web UI in the ETS section.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets_tab_map).

%% API
-export([update_node_ets_tab/3]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Updates the node ETS table section in Vizceral map.
-spec update_node_ets_tab(Node :: atom(), Proplist :: list(), Viz :: map()) -> 
                                 map().
update_node_ets_tab(Node, Proplist, Viz) ->
    ETSTabsMetric = get_ets_metric(Node, Proplist),
    create_ets_tab_map(Node, ETSTabsMetric, Viz).

%%====================================================================
%% Internals
%%====================================================================

get_ets_metric(Node, Proplist) ->
    Tabs = epl_ets_metric:get_node_ets_tabs(Node),
    TabsInfo = epl_ets_metric:get_ets_tabs_info(Node, Tabs),
    ETSCallTrace = proplists:get_value(ets_func, Proplist),
    TabsAccessTime = epl_ets_metric:get_ets_access_time(ETSCallTrace),
    [merge_metrics(Tab, [TabsInfo, TabsAccessTime]) || Tab <- Tabs].

merge_metrics(Tab, Metrics) ->
    TabMetric = lists:foldl(fun(Metric, Map) -> 
                                    maps:merge(Map, get_metric_val(Tab, Metric))
                            end, #{}, Metrics),
    #{namify(Tab) => TabMetric}.

get_metric_val(Tab, {Type, Metric}) ->
    #{namify(Type) => proplists:get_value(Tab, Metric)}.

create_ets_tab_map(Node, ETSTabsMetric, Viz) ->
    NewNodeTabs = #{name => epl_viz_map:namify(Node), tabs => ETSTabsMetric},
    {_, RestTabs} = pull_tab(Node, Viz, maps:is_key(ets_node_tabs, Viz)),
    maps:merge(Viz, #{ets_node_tabs => [NewNodeTabs | RestTabs]}).

pull_tab(Name, Entity, true) ->
    #{ets_node_tabs := NodeTabs} = Entity,
    {[NodeTab], Rest} = lists:partition(
                       fun(A) ->
                               maps:get(name, A) == namify(Name)
                       end, NodeTabs),
    {NodeTab, Rest};
pull_tab(_Name, _Entity, false) ->
    {null, []}.

namify(Name) ->
    epl_viz_map:namify(Name).

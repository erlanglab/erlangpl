%%%-------------------------------------------------------------------
%% @doc epl_ets_metric module.
%% This module provides an APIs for gathering and processing ETS related
%% metrics. It means information, data or statistics.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets_metric).

%% API
-export([get_node_ets_num/1,
         get_node_ets_mem/1,
         get_node_ets_tabs/1,
         get_ets_tabs_owner/2,
         get_ets_tabs_mem_sorted/2]).

%%====================================================================
%% API functions
%%====================================================================

get_node_ets_num(Node) ->
    {ok, AllETS} = epl:command(Node, fun ets:all/0, []),
    erlang:length(AllETS).

get_node_ets_mem(Node) ->
    {ok, MemoryData} = epl:command(Node, fun erlang:memory/0, []),
    MemoryPercent = proplists:get_value(ets, MemoryData) / 
        proplists:get_value(total, MemoryData),
    trunc_float(MemoryPercent, 4).

get_node_ets_tabs(Node) ->
    {ok, Tabs} = epl:command(Node, fun ets:all/0, []),
    Tabs.

get_ets_tabs_owner(Node, Tabs) ->
    lists:map(fun(T) -> get_ets_owner(Node, T) end, Tabs).

get_ets_tabs_mem_sorted(Node, Tabs) ->
    Tabs2 = lists:map(fun(T) -> get_ets_mem(Node, T) end, Tabs),
    lists:sort(fun({_T1, M1}, {_T2, M2}) -> M1 > M2 end, Tabs2).

%%====================================================================
%% Internals
%%====================================================================

get_ets_mem(Node, Tab) ->
    {ok, Mem}  = epl:command(Node, fun ets:info/2, [Tab, memory]),
    {Tab, Mem}.

get_ets_owner(Node, Tab) ->
    {ok, Owner}  = epl:command(Node, fun ets:info/2, [Tab, owner]),
    {Tab, Owner}.

trunc_float(Float, Pos) ->
    List = erlang:float_to_list(Float, [{decimals, Pos}]),
    erlang:list_to_float(List).

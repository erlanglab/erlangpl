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
         get_ets_tabs_mem/2,
         get_ets_tabs_size/2,
         get_ets_access_time/1]).

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
    TabsOwner = lists:map(fun(T) -> get_ets_owner(Node, T) end, Tabs),
    {owner, TabsOwner}.

get_ets_tabs_mem(Node, Tabs) ->
    TabsMem = lists:map(fun(T) -> get_ets_mem(Node, T) end, Tabs),
    {memory, TabsMem}.

get_ets_tabs_size(Node, Tabs) ->
    TabsSize = lists:map(fun(T) -> get_ets_size(Node, T) end, Tabs),
    {size, TabsSize}.

get_ets_access_time([]) ->
    {access_time, []};
get_ets_access_time(Traces) ->
    Traces2 = transform_traces_by_pid(Traces),
    Traces3 = transform_traces_by_tab(Traces2),
    {access_time, []}.

%%====================================================================
%% Internals
%%====================================================================

get_ets_mem(Node, Tab) ->
    {ok, Mem}  = epl:command(Node, fun ets:info/2, [Tab, memory]),
    {Tab, Mem}.

get_ets_size(Node, Tab) ->
    {ok, Size}  = epl:command(Node, fun ets:info/2, [Tab, size]),
    {Tab, Size}.

get_ets_owner(Node, Tab) ->
    {ok, Owner}  = epl:command(Node, fun ets:info/2, [Tab, owner]),
    {Tab, Owner}.

transform_traces_by_pid(Traces) ->
    TracesByPid = split_traces_by_pid(Traces),
    TracesSortedByMs = sort_traces_by_ms(TracesByPid),
    TracesCleaned = clean_traces_unpaired(TracesSortedByMs),
    TracesCallTime = calculate_calls_time(TracesCleaned),
    lists:merge(TracesCallTime).

transform_traces_by_tab(Traces) ->
    TracesByTab = split_traces_by_tab(Traces),
    TabFuncTime = format_traces_TFT(TracesByTab),
    io:format("~p~n", [TabFuncTime]).

split_traces_by_tab(Traces) ->
    TracesUniqueTabs = lists:ukeysort(2, Traces),
    Tabs = lists:map(fun({_, FT, _}) -> FT end, TracesUniqueTabs),
    Splitted = [lists:partition(fun({_, FT, _}) -> FT =:= PFT end, Traces) ||
                   PFT <- Tabs],
    lists:map(fun({S, _NS}) -> S end, Splitted).

split_traces_by_pid(Traces) ->
    TracesUniquePids = lists:ukeysort(1, Traces),
    Pids = lists:map(fun({Pid, _, _}) -> Pid end, TracesUniquePids),
    Splitted = [lists:partition(fun({Pid, _, _}) -> Pid =:= PPid end, Traces) ||
        PPid <- Pids],
    lists:map(fun({S, _NS}) -> S end, Splitted).

format_traces_TFT(TraceLists) ->
    [format_trace_TFT(T) || T <- TraceLists].

format_trace_TFT(Traces = [{_, {F,T}, _} | _]) ->
    {F,T, lists:map(fun({_, _, MS}) -> MS end, Traces)}.

clean_traces_unpaired(TraceLists) ->
    TL = [clean_first_trace(T) || T <- TraceLists],
    TLCleared = [clean_last_trace(T2, lists:last(T2)) || T2 <- TL],
    [lists:filter(fun(E) -> E =/= [] end, T3) || T3 <- TLCleared].

clean_first_trace([{_Pid, return_to, _TS} | Traces]) ->
    Traces;
clean_first_trace(Traces) ->
    Traces.

clean_last_trace(Traces, {_Pid, {_F,_A}, _TS}) ->
    lists:droplast(Traces);
clean_last_trace(Traces, _LT) ->
    Traces.

sort_traces_by_ms(TraceLists) ->
    TraceListsMs = [trace_ts_to_ms(TL) || TL <- TraceLists],
    [sort_trace_by_ms(TLMs) || TLMs <- TraceListsMs].

trace_ts_to_ms(Traces) ->
    lists:map(fun({Pid, F, TS}) -> {Pid, F, ts_to_ms(TS)} end, Traces).

ts_to_ms({Mega, Sec, Micro}) ->
    (Mega*1000000 + Sec)*1000 + Micro/1000.

sort_trace_by_ms(Traces) ->
    lists:sort(fun({_, _, M1}, {_, _, M2}) -> M1 < M2 end, Traces).

calculate_calls_time(TraceLists) ->
    [calculate_call_time(T) || T <- TraceLists].

calculate_call_time(Traces) ->
    PairStartPoint = lists:seq(1, erlang:length(Traces) - 1, 2),
    io:format("~p~n", [PairStartPoint]),
    %TPLists = [split_trace_pair(Traces, S) || S <- PairStartPoint],
    TPLists = [lists:sublist(Traces, S, 2) || S <- PairStartPoint],
    TPMergedLists = [merge_traces_pair(TP) || TP <- TPLists],
    lists:merge(TPMergedLists).

merge_traces_pair([{Pid, FA, TS1}, {_,_,TS2}]) ->
    [{Pid, FA, TS2 - TS1}].

trunc_float(Float, Pos) ->
    List = erlang:float_to_list(Float, [{decimals, Pos}]),
    erlang:list_to_float(List).

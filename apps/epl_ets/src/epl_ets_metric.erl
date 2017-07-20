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
         get_ets_tabs_info/2,
         get_ets_call_stats/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Calculates number of the ETS tables present on the `Node'.
-spec get_node_ets_num(Node :: atom()) -> integer().
get_node_ets_num(Node) ->
    {ok, AllETS} = epl:command(Node, fun ets:all/0, []),
    erlang:length(AllETS).

%% @doc Calculates allocated memory usage for the ETS tables present on the 
%% `Node'.
-spec get_node_ets_mem(Node :: atom()) -> float().
get_node_ets_mem(Node) ->
    {ok, MemoryData} = epl:command(Node, fun erlang:memory/0, []),
    MemoryPercent = proplists:get_value(ets, MemoryData) / 
        proplists:get_value(total, MemoryData),
    trunc_float(MemoryPercent, 4).

%% @doc Gets names of the ETS tables present on the `Node'.
-spec get_node_ets_tabs(Node :: atom()) -> [atom()].
get_node_ets_tabs(Node) ->
    {ok, Tabs} = epl:command(Node, fun ets:all/0, []),
    Tabs.

%% @doc Gets information about `Tabs' present on the `Node'.
-spec get_ets_tabs_info(Node :: atom(), Tabs :: [atom()]) -> 
    {info, [{atom(), map()}]}.
get_ets_tabs_info(Node, Tabs) ->
    TabsInfo = lists:map(fun(T) -> get_ets_info(Node, T) end, Tabs),
    {info, TabsInfo}.

%% @doc Calculates duration time statistics of ets insert/lookup functions calls. 
-spec get_ets_call_stats([] | list()) -> {call_stats, [] | [{atom(), list()}]}.
get_ets_call_stats([]) ->
    {call_stats, []};
get_ets_call_stats(Traces) ->
    Traces2 = transform_traces_by_pid(Traces),
    Traces3 = transform_traces_by_tab(Traces2),
    {call_stats, calculate_tabs_statistics(Traces3)}.

%%====================================================================
%% Internals
%%====================================================================

get_ets_info(Node, Tab) ->
    {ok, Info}  = epl:command(Node, fun ets:info/1, [Tab]),
    InfoMap = proplist_to_map(Info),
    {Tab, InfoMap}.

transform_traces_by_pid(Traces) ->
    TracesByPid = split_traces_by_pid(Traces),
    TracesSortedByMs = sort_traces_by_ms(TracesByPid),
    TracesCleaned = clean_traces_unpaired(TracesSortedByMs),
    TracesCallTime = calculate_calls_time(TracesCleaned),
    lists:merge(TracesCallTime).

transform_traces_by_tab(Traces) ->
    TracesByTab = split_traces_by_tab(Traces),
    format_traces_TFT(TracesByTab).

calculate_tabs_statistics(Stats) ->
    [calculate_tab_statistics(Stat) || Stat <- Stats].

calculate_tab_statistics({Tab, FuncStats}) ->
    {Tab, [calculate_tab_func_statistics(FuncStat) || FuncStat <- FuncStats]}.
    
calculate_tab_func_statistics({Func, TimeProbes}) ->
    StatsKey = [min, max, median, {percentile, [75, 90, 95, 99, 999]}],
    Stats = bear:get_statistics_subset(TimeProbes, StatsKey),
    #{<<"func">> => namify(Func), <<"time">> => stats_to_map(Stats), 
      <<"count">> => erlang:length(TimeProbes)}.

split_traces_by_tab(Traces) ->
    TracesUniqueTabs = lists:ukeysort(2, Traces),
    Tabs = lists:map(fun({_, T, _, _}) -> T end, TracesUniqueTabs),
    Splitted = [lists:partition(fun({_, T, _, _}) -> T =:= PT end, Traces) ||
                   PT <- Tabs],
    lists:map(fun({S, _NS}) -> S end, Splitted).

split_traces_by_pid(Traces) ->
    TracesUniquePids = lists:ukeysort(1, Traces),
    Pids = lists:map(fun({Pid, _, _, _}) -> Pid end, TracesUniquePids),
    Splitted = [lists:partition(fun({Pid, _, _, _}) -> Pid =:= PPid end, Traces) ||
        PPid <- Pids],
    lists:map(fun({S, _NS}) -> S end, Splitted).

split_traces_by_func(Traces) ->
    TracesUniqueFuncs = lists:ukeysort(3, Traces),
    Funcs = lists:map(fun({_, _, Func, _}) -> Func end, TracesUniqueFuncs),
    Splitted = [lists:partition(fun({_, _, Func, _}) -> Func =:= PFunc end, 
                                Traces) || PFunc <- Funcs],
    lists:map(fun({S, _NS}) -> S end, Splitted).

format_traces_TFT(TraceLists) ->
    [format_trace_TFT(T) || T <- TraceLists].

format_trace_TFT(Traces = [{_Pid, Tab, _Func, _T} | _]) ->
    TracesByFunc = split_traces_by_func(Traces),
    {Tab, [format_trace_TFT_by_func(T) || T <- TracesByFunc]}.

format_trace_TFT_by_func(Traces = [{_Pid, _Tab, Func, _T} | _]) ->
    {Func, lists:map(fun({_, _, _, MS}) -> trunc_float(MS, 4) end, Traces)}.

clean_traces_unpaired(TraceLists) ->
    TL = [clean_first_trace(T) || T <- TraceLists],
    TLCleared = [clean_last_trace(T2, lists:last(T2)) || T2 <- TL],
    [lists:filter(fun(E) -> E =/= [] end, T3) || T3 <- TLCleared].

clean_first_trace([{_Pid, null, return_to, _TS} | Traces]) ->
    Traces;
clean_first_trace(Traces) ->
    Traces.

clean_last_trace(Traces, {_Pid, null, return_to, _TS}) ->
    Traces;
clean_last_trace(Traces, _LT) ->
    lists:droplast(Traces).

sort_traces_by_ms(TraceLists) ->
    TraceListsMs = [trace_ts_to_ms(TL) || TL <- TraceLists],
    [sort_trace_by_ms(TLMs) || TLMs <- TraceListsMs].

trace_ts_to_ms(Traces) ->
    lists:map(fun({Pid, T, F, TS}) -> {Pid, T, F, ts_to_ms(TS)} end, Traces).

ts_to_ms({Mega, Sec, Micro}) ->
    (Mega*1000000 + Sec)*1000 + Micro/1000.

sort_trace_by_ms(Traces) ->
    lists:sort(fun({_, _, _, M1}, {_, _, _, M2}) -> M1 < M2 end, Traces).

calculate_calls_time(TraceLists) ->
    [calculate_call_time(T) || T <- TraceLists].

calculate_call_time(Traces) ->
    PairStartPoint = lists:seq(1, erlang:length(Traces) - 1, 2),
    TPLists = [lists:sublist(Traces, S, 2) || S <- PairStartPoint],
    TPMergedLists = [merge_traces_pair(TP) || TP <- TPLists],
    lists:merge(TPMergedLists).

merge_traces_pair([{Pid, T, F, TS1}, {_, _, _, TS2}]) ->
    [{Pid, T, F, TS2 - TS1}].

trunc_float(Float, Pos) ->
    List = erlang:float_to_list(Float, [{decimals, Pos}]),
    erlang:list_to_float(List).

namify_val(Val) when is_atom(Val) ->
    namify(Val);
namify_val(Val) ->
    Val.

namify(Name) ->
    epl_viz_map:namify(Name).

stats_to_map(Stats) ->
    StatsMap = proplist_to_map(Stats),
    Percentile = proplists:get_value(percentile, Stats),
    PercentileMap = proplist_to_map(Percentile),
    maps:put(namify(percentile), PercentileMap, StatsMap).

proplist_to_map(Proplist) ->
    lists:foldl(fun({Prop, Val}, Map) -> maps:put(namify(Prop), namify_val(Val), 
                                                  Map) end, #{}, Proplist).
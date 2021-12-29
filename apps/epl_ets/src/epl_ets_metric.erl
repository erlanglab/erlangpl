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
         get_ets_call_stats/1,
         get_ets_tab_traffic/1,
         trunc_float/2]).

%% Test
-export([split_tuple_list_by/2,
         sort_traces_by_ms/1,
         clean_traces_unpaired/1,
         calculate_call_time/1]).

%% Consts
-define(TRACE_PAIR_START_POINT, 1).
-define(TRACE_PAIR_END_POINT_FORMULA_FACTOR, 1).
-define(TRACE_PAIR_LENGTH, 2).

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
    calculate_memory_percent(MemoryData).

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

%% @doc Gets traffic information (number of inserts/lookups) about traced ETS
%% table.
-spec get_ets_tab_traffic([] | list()) -> {tab_traffic, [] | [{atom(), list()}]}.
get_ets_tab_traffic([]) ->
    {tab_traffic, []};
get_ets_tab_traffic(TrafficCounters) ->
    Tab = get_traffic_tab(TrafficCounters),
    TrafficCountersFlat = flat_traffic_counters(TrafficCounters),
    TrafficCountersByPid = split_traffic_counters_by_pid(TrafficCountersFlat),
    TrafficCountersFormatted = format_traffic_counters(TrafficCountersByPid),
    {tab_traffic, [{Tab, TrafficCountersFormatted}]}.


%% Truncs float number.
-spec trunc_float(Float :: float(), Pos :: integer()) -> float().
trunc_float(Float, Pos) ->
    List = erlang:float_to_list(Float, [{decimals, Pos}]),
    erlang:list_to_float(List).

%%====================================================================
%% Internals
%%====================================================================

calculate_memory_percent(MemoryData) ->
    MemoryPercent = proplists:get_value(ets, MemoryData) /
        proplists:get_value(total, MemoryData),
    trunc_float(MemoryPercent, 4).

get_ets_info(Node, Tab) ->
    {ok, Info}  = epl:command(Node, fun ets:info/1, [Tab]),
    if
        Info == undefined -> {Tab, #{}};
        true              -> InfoMap = proplist_to_map(Info),
                             {Tab, InfoMap}
    end.

transform_traces_by_pid(Traces) ->
    TracesByPid = split_traces_by_pid(Traces),
    TracesSortedByMs = sort_traces_by_ms(TracesByPid),
    TracesCleaned = clean_traces_unpaired(TracesSortedByMs),
    TracesCallTime = calculate_calls_time(TracesCleaned),
    lists:merge(TracesCallTime).

transform_traces_by_tab(Traces) ->
    TracesByTab = split_traces_by_tab(Traces),
    format_traces(TracesByTab).

calculate_tabs_statistics(Stats) ->
    [calculate_tab_statistics(Stat) || Stat <- Stats].

calculate_tab_statistics({Tab, FuncStats}) ->
    {Tab, [calculate_tab_func_statistics(FuncStat) || FuncStat <- FuncStats]}.
    
calculate_tab_func_statistics({Func, TimeProbes}) ->
    #{<<"func">> => namify(Func), <<"max_time">> => max(TimeProbes), 
      <<"count">> => erlang:length(TimeProbes)}.

split_traces_by_pid(Traces) ->
    split_tuple_list_by(Traces, 1).

split_traces_by_tab(Traces) ->
    split_tuple_list_by(Traces, 2).

split_traces_by_func(Traces) ->
    split_tuple_list_by(Traces, 3).

split_tuple_list_by(TupleList, ElemIndex) ->
    TuplesUnique = lists:ukeysort(ElemIndex, TupleList),
    KeysUnique = lists:map(fun(T) -> element(ElemIndex, T) end, TuplesUnique),
    Splitted = [lists:partition(fun(T) -> element(ElemIndex, T) =:= KU end,
                                TupleList) || KU <- KeysUnique],
    lists:map(fun({S, _NS}) -> S end, Splitted).

format_traces(TraceLists) ->
    [format_trace(T) || T <- TraceLists].

format_trace(Traces = [{_Pid, Tab, _Func, _T} | _]) ->
    TracesByFunc = split_traces_by_func(Traces),
    {Tab, [format_trace_by_func(T) || T <- TracesByFunc]}.

format_trace_by_func(Traces = [{_Pid, _Tab, Func, _T} | _]) ->
    {Func, lists:map(fun({_, _, _, MS}) -> trunc_float(MS, 4) end, Traces)}.

clean_traces_unpaired(TraceLists) ->
    TL = [clean_first_trace(T) || T <- TraceLists],
    TLCleared = [clean_last_trace(T2, get_last_elem(T2)) || T2 <- TL],
    [lists:filter(fun(E) -> E =/= [] end, T3) || T3 <- TLCleared].

clean_first_trace([{_Pid, null, return_to, _TS} | Traces]) ->
    Traces;
clean_first_trace(Traces) ->
    Traces.

clean_last_trace(Traces, null) ->
    Traces;
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
    PairStartPoint = lists:seq(?TRACE_PAIR_START_POINT,
                               get_trace_pair_endpoint(Traces),
                               ?TRACE_PAIR_LENGTH),
    TPLists = [lists:sublist(Traces, S, 2) || S <- PairStartPoint],
    TPMergedLists = [merge_traces_pair(TP) || TP <- TPLists],
    lists:merge(TPMergedLists).

get_trace_pair_endpoint(Traces) ->
    erlang:length(Traces) - ?TRACE_PAIR_END_POINT_FORMULA_FACTOR.

merge_traces_pair([{Pid, T, F, TS1}, {_, _, _, TS2}]) ->
    [{Pid, T, F, TS2 - TS1}].

max([]) ->
    0;
max(List) ->
    lists:max(List).

get_last_elem([]) ->
    null;
get_last_elem(List) ->
    lists:last(List).

namify_val(Val) when is_atom(Val) ->
    namify(Val);
namify_val(Val) ->
    Val.

namify(Name) ->
    epl_viz_map:namify(Name).


proplist_to_map({'EXIT', _reason}) -> #{};

proplist_to_map(Proplist) ->
    lists:foldl(fun({Prop, Val}, Map) -> maps:put(namify(Prop), namify_val(Val), 
                                                  Map) end, #{}, Proplist).

get_traffic_tab([{{_Pid, Tab, _Func}, _Count} | _]) ->
    Tab.

flat_traffic_counters(TrafficCounters) ->
    lists:map(fun({{Pid, Tab, Func}, Count}) -> {Pid, Tab, Func, Count} end,
              TrafficCounters).

split_traffic_counters_by_pid(TrafficCounters) ->
    split_tuple_list_by(TrafficCounters, 1).

format_traffic_counters(ListOfTrafficCounters) ->
    [merge_traffic_counters_pair(TC) || TC <- ListOfTrafficCounters].

merge_traffic_counters_pair(TrafficCounters = [{Pid, _, _, _} | _]) ->
    {Pid, [get_traffic_counter(C) || C <- TrafficCounters]}.

get_traffic_counter({_Pid, _Tab, Func, Counter}) ->
    {Func, Counter}.

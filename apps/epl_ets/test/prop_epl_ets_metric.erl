%%%-------------------------------------------------------------------
%% @doc epl_ets_metric_tests module.
%% This module contains tests for some of the functions from
%% epl_ets_metric module.
%% @end
%%%-------------------------------------------------------------------

-module(prop_epl_ets_metric).

-include_lib("proper/include/proper.hrl").

%%====================================================================
%% Property tests
%%====================================================================

prop_splitted_by() ->
    ?FORALL({Traces, ElemIndex}, {list_traces(), integer(1,3)},
            splitted_by(epl_ets_metric:split_traces_by(Traces, ElemIndex),
                        ElemIndex)).

prop_splitted_by_same_len() ->
    ?FORALL({Traces, ElemIndex}, {list_traces(), integer(1,3)},
            length_deep(epl_ets_metric:split_traces_by(Traces, ElemIndex)) ==
                length(Traces)).

prop_sorted_by_ms() ->
    ?FORALL(Traces, list(list_traces()),
            sorted_by_ms(epl_ets_metric:sort_traces_by_ms(Traces))).

prop_sorted_by_ms_same_len() ->
    ?FORALL(Traces, list(list_traces()),
           length_deep(epl_ets_metric:sort_traces_by_ms(Traces)) =:=
                length_deep(Traces)).

prop_cleared_unpaired() ->
    ?FORALL(Traces, list(list_traces_maybe_unpaired()),
            cleared_unpaired(epl_ets_metric:clean_traces_unpaired(Traces))).

prop_merged_pair() ->
    ?FORALL(Traces, list_traces_ready_to_merge(),
            length(epl_ets_metric:calculate_call_time(Traces)) =:=
                length(Traces) div 2).

%%====================================================================
%% Generators
%%====================================================================

list_traces() ->
    ?LET(T, list(trace_tuple()), T).

list_traces_maybe_unpaired() ->
    ?LET({Traces, T1, T2, T3, Variant},
         {list_traces(), trace_tuple(), trace_tuple(), trace_tuple(),
          integer(0,3)}, maybe_add_traces_unpaired(Traces, T1, T2, T3, Variant)).

list_traces_ready_to_merge() ->
    ?LET(Traces, list(trace_tuple_ms()), ensure_traces_even(Traces,
                                                         length(Traces) rem 2)).

trace_tuple() ->
    ?LET(T, tuple([integer(10000, 99999), atom(), atom(), timestamp()]),
         first_to_pid(T)).

trace_tuple_ms() ->
    ?LET(T, tuple([integer(10000, 99999), atom(), atom(), float(0, inf)]),
         first_to_pid(T)).

timestamp() ->
    ?LET(TS, tuple([integer(0, inf), integer(0, inf), integer(0, inf)]), TS).

%%====================================================================
%% Helpers
%%====================================================================

length_deep(ListOfLists) ->
    FlatList = lists:flatten(ListOfLists),
    length(FlatList).

first_to_pid({P, A1, A2, I}) ->
    {list_to_pid("<0." ++ integer_to_list(P) ++ ".0>"), A1, A2, I}.

splitted_by(ListOfLists, ElemIndex) ->
    Result = [is_elem_same(List, ElemIndex) || List <- ListOfLists],
    lists:all(fun(E) -> E end, Result).

is_elem_same(List = [First | _Rest], ElemIndex) ->
    BaseElem = element(ElemIndex, First),
    lists:all(fun(E) -> element(ElemIndex, E) =:= BaseElem end, List).

sorted_by_ms(ListOfLists) ->
    Result = [is_list_sorted(List) || List <- ListOfLists],
    lists:all(fun(E) -> E end, Result).

is_list_sorted(List) ->
    MiliSecSet = lists:map(fun(E) -> element(4, E) end, List),
    MiliSecSet =:= lists:sort(MiliSecSet).

maybe_add_traces_unpaired(Traces, T1, T2, {Pid, _, _, TS}, Variant) ->
    TracesProperLast = Traces ++ [{Pid, null, return_to, TS}],
    case Variant of
        0 -> TracesProperLast;
        1 -> add_unpaired_trace_as_first(T1, TracesProperLast);
        2 -> add_unpaired_trace_as_last(T2, TracesProperLast);
        3 ->
            add_unpaired_trace_as_first(T1, TracesProperLast),
            add_unpaired_trace_as_last(T2, TracesProperLast)
    end.

add_unpaired_trace_as_first({Pid, _, _, TS}, Traces) ->
    [{Pid, null, return_to, TS} | Traces].

add_unpaired_trace_as_last(Trace, Traces) ->
    Traces ++ [Trace].

cleared_unpaired(ListOfTraces) ->
    Result = [are_traces_paired(Traces) || Traces <- ListOfTraces],
    lists:all(fun(E) -> E end, Result).

are_traces_paired([]) ->
    true;
are_traces_paired([_]) ->
    true;
are_traces_paired([{_, Func, Call, _} | Rest]) ->
    TraceLast = lists:last(Rest),
    FuncLast = element(2, TraceLast),
    CallLast = element(3, TraceLast),
    if Func =/= null, Call =/= return_to, FuncLast =:= null, CallLast =:= return_to ->
            true;
       true -> false
    end.

ensure_traces_even(Traces, 0) ->
    Traces;
ensure_traces_even([First | Rest], _) ->
    Rest.

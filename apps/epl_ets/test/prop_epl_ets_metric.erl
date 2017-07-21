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

prop_splitted_by_pid() ->
    ?FORALL(T, list_traces(), 
            splitted_by_pid(epl_ets_metric:split_traces_by_pid(T))).

prop_splitted_same_len() ->
    ?FORALL(T, list_traces(),
            length_deep(epl_ets_metric:split_traces_by_pid(T)) == length(T)).

%%====================================================================
%% Generators
%%====================================================================

list_traces() ->
    ?LET(T, list(trace_tuple(integer(10000,99999), atom(), atom(), integer())),
         T).

trace_tuple(I, A1, A2, I2) ->
    ?LET(T, tuple([I, A1, A2, I2]), first_to_pid(T)).

%%====================================================================
%% Helpers
%%====================================================================

length_deep(ListOfLists) ->
    FlatList = lists:flatten(ListOfLists),
    length(FlatList).

first_to_pid({P, A1, A2, I}) ->
    {list_to_pid("<0." ++ integer_to_list(P) ++ ".0>"), A1, A2, I}.

splitted_by_pid(ListOfLists) ->
    Result = [is_pid_same(List) || List <- ListOfLists],
    lists:all(fun(E) -> E end, Result).

is_pid_same(List = [{Pid, _, _, _} | _]) ->
    lists:all(fun({P, _, _, _}) -> P =:= Pid end, List).

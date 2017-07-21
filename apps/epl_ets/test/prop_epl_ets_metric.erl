%%%-------------------------------------------------------------------
%% @doc epl_ets_metric_tests module.
%% This module contains tests for some of the functions from
%% epl_ets_metric module.
%% @end
%%%-------------------------------------------------------------------

-module(prop_epl_ets_metric).

-include_lib("proper/include/proper.hrl").

prop_splitted_by_pid() ->
    ?FORALL(T, list_traces(), 
            splitted_by_pid(epl_ets_metric:split_traces_by_pid(T))).

prop_splitted_same_len() ->
    ?FORALL(T, list_traces(),
            length(lists:flatten(epl_ets_metric:split_traces_by_pid(T))) ==
                length(T)).

list_traces() ->
    ?LET(T, list(tuple([integer(10000,99999),
                        atom(),
                        atom(),
                        integer()])), first_to_pid(T)).

first_to_pid(L) ->
    lists:map(fun({P, X, Y, Z}) -> 
                      {erlang:list_to_pid("<0." ++ erlang:integer_to_list(P) ++ 
                                              ".0>"), X, Y, Z} end, L).

splitted_by_pid(ListOfLists) ->
    Result = [is_pid_same(List) || List <- ListOfLists],
    lists:all(fun(E) -> E end, Result).

is_pid_same(List = [{Pid, _, _, _} | _]) ->
    lists:all(fun({P, _, _, _}) -> P =:= Pid end, List).

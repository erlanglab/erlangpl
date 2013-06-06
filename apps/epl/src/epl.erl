%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl).
-include_lib("epl/include/epl.hrl").

-export([lookup/1,
         subscribe/0,
         subscribe/1,
         unsubscribe/0,
         unsubscribe/1,
         trace_pid/1,
         proplist_to_json/1,
         to_bin/1,
         log/3,
         timestamp/1,
         process_info/1
        ]).

%% ===================================================================
%% API functions
%% ===================================================================
lookup(Key) ->
    ets:lookup(epl_priv, Key).

subscribe() ->
    epl_tracer:subscribe().

subscribe(Pid) ->
    epl_tracer:subscribe(Pid).

unsubscribe() ->
    epl_tracer:unsubscribe().

unsubscribe(Pid) ->
    epl_tracer:unsubscribe(Pid).

trace_pid(Pid) ->
    epl_tracer:trace_pid(Pid).

proplist_to_json(Term) ->
    Obj = encode(Term, ej:new()),
    ej:encode(Obj).

to_bin(I) when is_atom(I) -> list_to_binary(atom_to_list(I));
to_bin(I) when is_integer(I) -> list_to_binary(integer_to_list(I));
to_bin(I) when is_pid(I) -> list_to_binary(pid_to_list(I));
to_bin(I) when is_port(I) -> list_to_binary(erlang:port_to_list(I));
to_bin(I) when is_list(I) -> list_to_binary(I);
to_bin(I) when is_binary(I) -> I.

log(Level, Str, Args) ->
    [{log_level, LogLevel}] = ets:lookup(epl_priv, log_level),
    case should_log(LogLevel, Level) of
        true ->
            io:format(log_prefix(Level) ++ Str, Args);
        false ->
            ok
    end.

timestamp(TS) ->
    {{Y,M,D},{H,Mi,S}} = calendar:now_to_local_time(TS),
    DT = [integer_to_list(Y),
          io_lib:format("~.2.0w",[M]),
          io_lib:format("~.2.0w",[D]),
          $_,
          io_lib:format("~.2.0w",[H]),
          io_lib:format("~.2.0w",[Mi]),
          io_lib:format("~.2.0w",[S])],
    to_bin(DT).

process_info(Pid) ->
    {ok, rpc:call(node(Pid), erlang, process_info, [Pid])}.

%% ===================================================================
%% internal functions
%% ===================================================================

%% if sub-proplist present, nest it under Key
encode([{Key,V}|Rest], Obj) ->
    Obj1 = encode([to_bin(Key)], V, Obj),
    encode(Rest, Obj1);
%% if item is other than 2-element tuple, skip it
encode([I|Rest], Obj) ->
    ?DEBUG("Skipping: ~p~n", [I]),
    encode(Rest, Obj);
encode([], Obj) ->
    Obj.

encode(Key, [{SubKey,V}|Rest], Obj) ->
    Obj1 = encode(Key++[to_bin(SubKey)], V, Obj),
    encode(Key, Rest, Obj1);
encode(_, [], Obj) ->
    Obj;
encode(Key, V, Obj) when is_integer(hd(V)); is_atom(V) ->
    ej:set_p(list_to_tuple(Key), Obj, to_bin(V));
encode(Key, V, Obj) when is_binary(V); is_integer(V); is_float(V) ->
    ej:set_p(list_to_tuple(Key), Obj, V);
encode(Key, [I|Rest], Obj) ->
    ?DEBUG("Skipping: ~p~n", [I]),
    encode(Key, Rest, Obj);
encode(Key, V, Obj) when not is_tuple(V) ->
    ej:set_p(list_to_tuple(Key), Obj, to_bin(V));
encode(_Key, I, Obj) ->
    ?DEBUG("Skipping: ~p~n", [I]),
    Obj.

should_log(debug, _)     -> true;
should_log(info, debug)  -> false;
should_log(info, _)      -> true;
should_log(warn, debug)  -> false;
should_log(warn, info)   -> false;
should_log(warn, _)      -> true;
should_log(error, error) -> true;
should_log(error, _)     -> false;
should_log(_, _)         -> false.

log_prefix(debug) -> "DEBUG: ";
log_prefix(info)  -> "INFO:  ";
log_prefix(warn)  -> "WARN:  ";
log_prefix(error) -> "ERROR: ".

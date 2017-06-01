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
         subscribe/2,
         unsubscribe/0,
         unsubscribe/1,
         unsubscribe/2,
         process_info/1,
         trace_pid/1,
         to_bin/1,
         log/3,
         timestamp/1
        ]).

%% ===================================================================
%% API functions
%% ===================================================================
lookup(Key) ->
    ets:lookup(epl_priv, Key).

subscribe() ->
    epl_tracer:subscribe().

subscribe(Node) ->
    epl_tracer:subscribe(Node).

subscribe(Node, Pid) ->
    epl_tracer:subscribe(Node, Pid).

unsubscribe() ->
    epl_tracer:unsubscribe().

unsubscribe(Node) ->
    epl_tracer:unsubscribe(Node).

unsubscribe(Node, Pid) ->
    epl_tracer:unsubscribe(Node, Pid).

process_info(Pid) ->
    Node = erlang:node(Pid),
    epl_tracer:command(Node, fun erlang:process_info/1, [Pid]).

trace_pid(Pid) ->
    epl_tracer:trace_pid(Pid).

to_bin(I) when is_atom(I)      -> list_to_binary(atom_to_list(I));
to_bin(I) when is_integer(I)   -> list_to_binary(integer_to_list(I));
to_bin(I) when is_pid(I)       -> list_to_binary(pid_to_list(I));
to_bin(I) when is_port(I)      -> list_to_binary(erlang:port_to_list(I));
to_bin(I) when is_reference(I) -> list_to_binary(erlang:ref_to_list(I));
to_bin(I) when is_float(I)     -> list_to_binary(float_to_list(I));
to_bin(I) when is_function(I)  -> list_to_binary(erlang:fun_to_list(I));
to_bin(I) when is_binary(I)    -> I;
to_bin(I) when is_list(I)      -> case catch list_to_binary(I) of
                                      {'EXIT', {badarg,_}} ->
                                          [to_bin(El) || El <- I];
                                      B ->
                                          B
                                  end.

log(Level, Str, Args) ->
    [{log_level, LogLevel}] = lookup(log_level),
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

%% ===================================================================
%% internal functions
%% ===================================================================

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

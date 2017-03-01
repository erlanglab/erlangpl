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
         process_info/1,
         trace_pid/1,
         proplist_to_json/2,
         to_bin/1,
         log/3,
         timestamp/1,
         add_plugin_menu/1
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

process_info(Pid) ->
    epl_tracer:process_info(Pid).

trace_pid(Pid) ->
    epl_tracer:trace_pid(Pid).

proplist_to_json(Term, Topic) ->
    Obj = encode(Term, ej:new()),
    Data = [{<<"topic">>, Topic},
            {<<"data">>, Obj}],
    ej:encode(Data).

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


add_plugin_menu(MenuItem) ->
    IndexHtml = <<"epl/priv/htdocs/index.html">>,
    [{_, Bin}] = ets:lookup(epl_priv, IndexHtml),
    %% Look for predefined string in HTML source code
    %% and add there HTML code with Plugin menu item
    NewBin = re:replace(Bin, <<"<!--PLUGIN_MENU-->">>,
                        <<MenuItem/binary, $&>>,
                        [{return,binary}]),
    ets:insert(epl_priv, {IndexHtml, NewBin}).

%% ===================================================================
%% internal functions
%% ===================================================================

encode(Proplist, Obj) when is_list(Proplist) ->
    encode([], Proplist, Obj).

encode(_, [], Obj) ->
    Obj;
encode(Key, [{SubKey,V}|Rest], Obj) ->
    %% if sub-proplist present, nest it under SubKey
    case catch to_bin(SubKey) of
        {'EXIT', _} ->
            ?DEBUG("Skipping: ~p~n", [{SubKey,V}]),
            encode(Key, Rest, Obj);
        SubKeyBin ->
            Obj1 = encode(Key++[SubKeyBin], V, Obj),
            encode(Key, Rest, Obj1)
    end;
encode(Key, V, Obj) when is_integer(hd(V)); not is_tuple(V) ->
    case catch to_bin(V) of
        {'EXIT', _} ->
            ?DEBUG("Skipping: ~p~n", [V]),
            Obj;
        Vbin ->
            ej:set_p(list_to_tuple(Key), Obj, Vbin)
    end;
encode(Key, [I|Rest], Obj) ->
    ?DEBUG("Skipping: ~p~n", [I]),
    encode(Key, Rest, Obj);
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

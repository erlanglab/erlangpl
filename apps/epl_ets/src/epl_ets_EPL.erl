%%%-------------------------------------------------------------------
%% @doc epl_ets_EPL module.
%% It is a cowboy websocket handler module which receives data from 
%% epl_ets and sends them throught the websocket.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets_EPL).

-behaviour(cowboy_websocket_handler).

%% cowboy_websocket_handler callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%%====================================================================
%% cowboy_websocket_handler callbacks
%%====================================================================

init({tcp, http}, _Req, _Opts) ->
    epl_ets:subscribe(),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined}.

websocket_handle({text, MsgBin}, Req, State) ->
    Msg = epl_json:decode(MsgBin),
    dispatch_request(Msg),
    {ok, Req, State};
websocket_handle(Data, _Req, _State) ->
    exit({not_implemented, Data}).

websocket_info({data, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};
websocket_info(Info, _Req, _State) ->
    exit({not_implemented, Info}).

websocket_terminate(_Reason, _Req, _State) ->
    epl_ets:unsubscribe(),
    [epl_tracer:disable_ets_call_tracing(N) || N <- nodes()],
    ok.

%%====================================================================
%% Internals
%%====================================================================

dispatch_request(#{<<"enable">> := true, <<"node">> := NodeBin,
                   <<"table">> := TabBin}) ->
    Node = binary_to_existing_atom(NodeBin, latin1),
    TabId = binary_to_tab_id(TabBin),
    ok = trace_ets_table(Node, TabId);
dispatch_request(#{<<"enable">> := true, <<"node">> := NodeBin}) ->
    Node = binary_to_existing_atom(NodeBin, latin1),
    ok = epl_tracer:enable_ets_call_tracing(Node);
dispatch_request(#{<<"enable">> := false, <<"node">> := NodeBin}) ->
    Node = binary_to_existing_atom(NodeBin, latin1),
    ok = epl_tracer:disable_ets_call_tracing(Node).

binary_to_tab_id(TabBin) ->
    try
        binary_to_integer(TabBin)
    catch
        error:badarg ->
            convert_binary_to_tab_name(TabBin)
    end.

convert_binary_to_tab_name(TabBin) ->
    try
        binary_to_existing_atom(TabBin, latin1)
    catch
        error:badarg ->
            undefined
    end.

trace_ets_table(_Node, undefined) ->
    ok;
trace_ets_table(Node, TabId) ->
    ok = epl_tracer:enable_ets_table_call_tracing(Node, TabId).

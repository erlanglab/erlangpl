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

-record(state, {ets_call_traced_nodes = []}).

%%====================================================================
%% cowboy_websocket_handler callbacks
%%====================================================================

init({tcp, http}, _Req, _Opts) ->
    epl_ets:subscribe(),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #state{}}.

websocket_handle({text, NodeBin}, Req, 
                 State = #state{ets_call_traced_nodes = TNodes}) ->
    Node = erlang:binary_to_existing_atom(NodeBin, latin1),
    NewTNodes = handle_ets_call_tracing(Node, TNodes,
                                        lists:member(Node, TNodes)),
    NewState = State#state{ets_call_traced_nodes = NewTNodes},
    {ok, Req, NewState};
websocket_handle(Data, _Req, _State) ->
    exit({not_implemented, Data}).

websocket_info({data, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};
websocket_info(Info, _Req, _State) ->
    exit({not_implemented, Info}).

websocket_terminate(_Reason, _Req, #state{ets_call_traced_nodes = TNodes}) ->
    epl_ets:unsubscribe(),
    [epl_tracer:disable_ets_call_tracing(N) || N <- TNodes],
    ok.

%%====================================================================
%% Internals
%%====================================================================

handle_ets_call_tracing(Node, Nodes, true) ->
    ok = disable_ets_call_tracing(Node, whereis(Node)),
    lists:delete(Node, Nodes);
handle_ets_call_tracing(Node, Nodes, false) ->
    ok = enable_ets_call_tracing(Node, whereis(Node)),
    [Node | Nodes].

enable_ets_call_tracing(_Node, undefined) ->
    ok;
enable_ets_call_tracing(Node, _) ->
    epl_tracer:enable_ets_call_tracing(Node).

disable_ets_call_tracing(_Node, undefined) ->
    ok;
disable_ets_call_tracing(Node, _) ->
    epl_tracer:disable_ets_call_tracing(Node).

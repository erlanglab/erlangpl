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
    {ok, Req, undefined_state}.

websocket_handle(Data, _Req, _State) ->
    exit({not_implemented, Data}).

websocket_info({data, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};
websocket_info(Info, _Req, _State) ->
    exit({not_implemented, Info}).

websocket_terminate(_Reason, _Req, _State) ->
    epl_ets:unsubscribe(),
    ok.

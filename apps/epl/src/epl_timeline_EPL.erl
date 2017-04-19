%%% Copyright (c) 2017, erlang.pl
%%%-------------------------------------------------------------------
%%% @doc
%%% WebSocket handler returning info from timeline observers
%%% @end
%%%-------------------------------------------------------------------
-module(epl_timeline_EPL).
-behaviour(cowboy_websocket_handler).

%% cowboy_websocket_handler callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%%%===================================================================
%%% cowboy_websocket_handler callbacks
%%%===================================================================
init({tcp, http}, _Req, _Opts) ->
    epl_timeline:subscribe(),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, Data}, Req, State) ->
    case epl_json:decode(Data) of
        [<<"add">>, Pid] -> epl_timeline:handle_pid(add, Pid);
        [<<"remove">>, Pid] -> epl_timeline:handle_pid(remove, Pid);
        _ -> invalid_msg
    end,
    {ok, Req, State};

websocket_handle(Data, _Req, _State) ->
    exit({not_implemented, Data}).

websocket_info({data, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};
websocket_info(Info, _Req, _State) ->
    exit({not_implemented, Info}).

websocket_terminate(_Reason, _Req, _State) ->
    epl_timeline:unsubscribe(),
    ok.

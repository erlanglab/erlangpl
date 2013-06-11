%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_dashboard_EPL).
-behaviour(cowboy_websocket_handler).

%% EPL plugin callbacks
-export([start_link/1,
         init/1]).

%% cowboy_websocket_handler callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%%%===================================================================
%%% EPL plugin callbacks
%%%===================================================================
start_link(_Options) ->
    {ok, spawn(fun() -> receive _ -> ok end end)}.

init(_Options) ->
    MenuItem = <<"<li class=\"glyphicons display\">",
                 "<a href=\"/epl_dashboard.html\"><i></i><span>Dashboard</span></a>",
                 "</li>">>,
    Author = <<"Erlang Lab">>,
    {ok, [{menu_item, MenuItem}, {author, Author}]}.

%%%===================================================================
%%% cowboy_websocket_handler callbacks
%%%===================================================================
init({tcp, http}, _Req, _Opts) ->
    [{node, Node}] = epl:lookup(node),
    [{node_settings, NodeSettings}] = epl:lookup(node_settings),
    JSON = epl:proplist_to_json([{node_name, Node}|NodeSettings]),
    self() ! {data, JSON},
    epl_dashboard:subscribe(),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({data, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

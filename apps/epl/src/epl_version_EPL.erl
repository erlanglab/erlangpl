%%% Copyright (c) 2017, erlang.pl
%%%-------------------------------------------------------------------
%%% @doc
%%% GET handler returning versions of applications
%%% @end
%%%-------------------------------------------------------------------

-module(epl_version_EPL).

%% EPL plugin callbacks
-export([start_link/1,
         init/1]).

%% cowboy handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

%%%===================================================================
%%% EPL plugin callbacks
%%%===================================================================
start_link(_Options) ->
    {ok, spawn(fun() -> receive _ -> ok end end)}.

init(_Options) ->
    {ok, [{menu_item, <<"">>}, {author, <<"">>}]}.

%%%===================================================================
%%% cowboy handler callbacks
%%%===================================================================
init(_Transport, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Vsn} = application:get_key(epl, vsn),
    JSON = binary:list_to_bin(["{\"version\":\"", Vsn, "\"}"]),
    {ok, Req1} = cowboy_req:reply(200, [], JSON, Req),

    {ok, Req1, State}.

terminate(_Reason, Req, _State) ->
    Req.

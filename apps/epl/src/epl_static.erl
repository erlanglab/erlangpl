%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_static).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Html = get_file(<<"index.html">>),
    {ok, Req2} = cowboy_req:reply(
                   200, [{<<"content-type">>, <<"text/html">>}], Html, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

get_file(Path) ->
    [{_, Bin}] = ets:lookup(epl_priv,
                            <<"epl/priv/htdocs/", Path/binary>>),
    Bin.

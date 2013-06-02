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

init(_Transport, Req, App) when is_list(App) ->
    {ok, Req, << (list_to_binary(App))/binary, "/priv/htdocs/" >>}.

handle(Req, PathPrefix) ->
    {PathInfo,_} = cowboy_req:path_info(Req),
    File = filename:join(PathInfo),
    Html = get_file(<< PathPrefix/binary, File/binary >>),
    {ok, Req2} = cowboy_req:reply(
                   200, [{<<"content-type">>, <<"text/html">>}], Html, Req),
    {ok, Req2, PathPrefix}.

terminate(_Reason, _Req, _State) ->
    ok.

get_file(Path) ->
    [{_, Bin}] = ets:lookup(epl_priv, Path),
    Bin.

%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(erlangpl).
-include_lib("epl/include/epl.hrl").

-export([main/1]).

main(_) ->
    {ok, _} = application:ensure_all_started(epl),
    {ok, _} = application:ensure_all_started(epl_st),

    %% Start applications because escript can't take -boot argument
    %% TODO: start sasl if escript run with debug flags
    %% ok = application:start(sasl),

    %% let escript process sleep, so that we don't stop
    receive _ -> ok end.

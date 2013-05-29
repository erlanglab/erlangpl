%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(erlangpl).
-export([main/1]).

main(_) ->
    %% Start applications because escript can't take -boot argument
    %% TODO: start sasl if escript run with debug flags
    %% ok = application:start(sasl),
    ok = application:start(crypto),
    ok = application:start(getopt),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(ej),
    ok = application:start(epl),

    %% TODO: show which_applications if escript run with debug flags
    %% io:format("which_applications: ~p~n", [application:which_applications()]),

    %% let escript process sleep, so that we don't stop
    receive _ -> ok end.

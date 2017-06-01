%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, A, T), {I, {I, start_link, A}, permanent, 5000, T, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Module, Args, Type) ->
    supervisor:start_child(?MODULE, ?CHILD(Module, Args, Type)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

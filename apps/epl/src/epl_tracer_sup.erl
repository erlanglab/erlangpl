%%%-------------------------------------------------------------------
%% @doc epl_tracer_sup module.
%% Supervises epl_tracer gen_servers.
%% @doc end
%%%-------------------------------------------------------------------

-module(epl_tracer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts epl_tracer_sup supervisor.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts epl_tracer_sup child.
-spec start_child(Args :: list()) -> 
    {ok, term()} | {ok, term(), term()} | {error, term()}.
start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    SupFlags = {simple_one_for_one, 5, 10},
    ChildSpec = [#{id => epl_tracer,
                   start => {epl_tracer, start_link, []},
                   restart => temporary,
                   shutdown => 5000,
                   type => worker,
                   module => [epl_tracer]}],
    {ok, {SupFlags, ChildSpec}}.

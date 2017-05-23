%%%-------------------------------------------------------------------
%% @doc epl_ets top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpec = [#{id => epl_ets,
                   start => {epl_ets, start_link, []}}],
    {ok, {SupFlags, ChildSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================

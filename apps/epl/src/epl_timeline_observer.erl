-module(epl_timeline_observer).
-behaviour(gen_server).

-include_lib("epl/include/epl.hrl").

-export([
         start_link/1,
         timeline/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


-record(state, {
          changes = [],
          tracked_pid
         }).
-record(revolution, {
          state,
          message
         }).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Pid) ->
    gen_server:start_link(?MODULE, Pid, []).

timeline(Tracker) ->
    gen_server:call(Tracker, timeline).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Pid) ->
    epl_tracer:subscribe(),
    epl_tracer:trace_pid(Pid),
    {ok, #state{tracked_pid = Pid}}.

handle_call(timeline, _, State = #state{changes=Changes}) ->
    {reply, Changes, State}.

handle_info(Message, State) ->
    io:fwrite("~62p~n",[Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

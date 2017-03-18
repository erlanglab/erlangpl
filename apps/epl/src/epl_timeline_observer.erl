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
    epl_tracer:track_timeline(Pid),
    {ok, #state{tracked_pid = Pid}}.

handle_call(timeline, _, State = #state{changes=Changes}) ->
    {reply, Changes, State}.

handle_info({data, _, Data}, State = #state{changes = Changes, tracked_pid = PID}) ->
    Merged = parse_data(Data, PID) ++ Changes,
    {noreply, State#state{changes = Merged}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_data(Data,Pid) ->
    case lists:keyfind(timeline, 1, Data) of
        false -> [];
        [] -> [];
        {timeline, Timelines } ->
            io:fwrite("~62p | ~62p ~n", [Pid, Timelines]),
            case lists:keyfind(Pid, 1, Timelines) of
                false ->
                    ["kurwa"];
                RawData -> RawData
            end
    end.

-module(epl_timeline_observer).
-behaviour(gen_server).

-include_lib("epl/include/epl.hrl").

-export([
         start_link/1,
         timeline/1]).

-export([
         init/1,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {
          changes = [],
          tracked_pid,
          pid_as_list}).

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
    {ok, #state{tracked_pid = Pid, pid_as_list = pid_to_list(Pid) }}.

handle_call(timeline, _, State = #state{changes=Changes}) ->
    {reply, Changes, State}.

handle_info({data, _, Data}, State = #state{changes = Changes, pid_as_list = PID}) ->
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
    ListOfProcessHistories = strip_to_timeline(Data),
    strip_to_raw_data(ListOfProcessHistories, Pid).

strip_to_timeline(Data) ->
    case lists:keyfind(timeline, 1, Data) of
        false -> [];
        [] -> [];
        {timeline, Timelines } -> Timelines
    end.

strip_to_raw_data(Data, Pid) ->
    case lists:keyfind(Pid, 1, Data) of
        false -> [];
        {_,RawData} -> RawData
    end.

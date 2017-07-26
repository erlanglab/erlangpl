%%% Copyright (c) 2017, erlang.pl
%%%-------------------------------------------------------------------
%%% @doc
%%% Tracking all timeline observers
%%% @end
%%%-------------------------------------------------------------------
-module(epl_timeline).
-behaviour(gen_server).

%% API
-export([start_link/0,
         subscribe/0,
         unsubscribe/0,
         handle_pid/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {subscribers = [],
                timelines = []}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe() ->
    gen_server:cast(?MODULE, {subscribe, self()}).

unsubscribe() ->
    gen_server:cast(?MODULE, {unsubscribe, self()}).

handle_pid(Action, P) when is_list(P) ->
    try list_to_pid(P) of
        Pid -> gen_server:cast(?MODULE, {Action, Pid})
    catch
        error:badarg -> noop
    end;

handle_pid(Action, Pid) when is_binary(Pid) ->
    handle_pid(Action, binary_to_list(Pid)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = epl:subscribe(default_node),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({subscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = [Pid|Subs]}};
handle_cast({unsubscribe, Pid}, State = #state{subscribers = Subs}) ->
    NextSubs = lists:delete(Pid, Subs),
    {noreply, State#state{subscribers = NextSubs}};
handle_cast({add, Pid}, State = #state{timelines = Timelines}) when is_pid(Pid) ->
    T = case lists:any(fun({P,_,_}) -> P == Pid end, Timelines) of
        true -> Timelines;
        _    -> [track_timeline(Pid)|Timelines]
        end,
    {noreply, State#state{timelines = T}};
handle_cast({remove, Pid}, State = #state{timelines = Timelines}) when is_pid(Pid) ->
    NextTimelines = lists:filter(fun({P,_,_}) -> P /= Pid end, Timelines),
    {noreply, State#state{timelines = NextTimelines}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({data, _, Data}, State = #state{subscribers = Subs, timelines = Timelines}) ->
    {Maped, Updated} = lists:foldr(fun (T, A) -> update_timeline(T, A, Data) end, {[], []}, Timelines),
    JSON = epl_json:encode(#{timelines => Maped}, <<"timeline-info">>),
    [Pid ! {data, JSON} || Pid <- Subs],
    {noreply, State#state{timelines = Updated}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_timeline({Pid, PidList, Changes}, {Maped, Updated}, Data) ->
    Merged = merge_changes(Data, PidList, Changes),
    Map = #{pid => epl:to_bin(Pid),
            timeline => lists:map(fun({M, S}) ->
                                          #{message => to_string(M),
                                            state => to_string(S)}
                                  end, Merged)},
    {[Map|Maped], [{Pid, PidList, Merged}|Updated]}.

track_timeline(Pid) ->
    epl_tracer:track_timeline(Pid),
    {Pid, pid_to_list(Pid), []}.

to_string(Data) ->
    S = io_lib:format("~p",[Data]),
    epl:to_bin(lists:flatten(S)).

merge_changes(Data, Pid, Changes) ->
    parse_data(Data, Pid) ++ Changes.

parse_data(Data, Pid) ->
    ListOfProcessHistories = strip_to_timeline(Data),
    strip_to_raw_data(ListOfProcessHistories, Pid).

strip_to_timeline(Data) ->
    case lists:keyfind(timeline, 1, Data) of
        false -> [];
        [] -> [];
        {timeline, Timelines} -> Timelines
    end.

strip_to_raw_data(Data, Pid) ->
    case lists:keyfind(Pid, 1, Data) of
        false -> [];
        {_,RawData} -> RawData
    end.

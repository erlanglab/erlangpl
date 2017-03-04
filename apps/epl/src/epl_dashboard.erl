%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_dashboard).
-behaviour(gen_server).

%% API
-export([start_link/0,
         subscribe/0,
         unsubscribe/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {subscribers = []}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

subscribe() ->
    gen_server:cast(?MODULE, {subscribe, self()}).

unsubscribe() ->
    gen_server:cast(?MODULE, {unsubscribe, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(no_args) ->
    ok = epl:subscribe(),
    {ok, #state{}}.

handle_call(Request, _From, _State) ->
    exit({not_implemented, Request}).

handle_cast({subscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = [Pid|Subs]}};
handle_cast({unsubscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};
handle_cast(Request, _State) ->
    exit({not_implemented, Request}).


handle_info({data, {N, T}, Proplist}, State = #state{subscribers = Subs}) ->
    Proplist1 =
        lists:map(fun({spawn, Spawn}) ->
                          {spawn, [{<<"count">>, epl:to_bin(length(Spawn))}]};
                     ({exit, Exit}) ->
                          Abnormal = lists:foldl(
                                       fun({_,normal,_}, Acc) -> Acc;
                                          (_, Acc) -> Acc + 1
                                       end,
                                       0, Exit),
                          {exit, [{<<"count">>, epl:to_bin(length(Exit))},
                                  {<<"abnormal">>, epl:to_bin(Abnormal)}]};
                     ({'receive', Receive}) ->
                          {Count, Sizes} = lists:foldl(
                                             fun({_,C,S}, {Count, Sizes}) ->
                                                     {Count+C, Sizes+S}
                                             end,
                                             {0, 0}, Receive),
                          {'receive', [{<<"count">>, epl:to_bin(Count)},
                                       {<<"sizes">>, epl:to_bin(Sizes)}]};
                     ({process_count,_} = Item) ->
                          Item;
                     ({memory_total,_} = Item) ->
                          Item;
                     ({Item,_}) ->
                          {Item,[]}
                  end,
                  Proplist),

    Id = << (epl:to_bin(N))/binary, $:, (epl:timestamp(T))/binary >>,
    Proplist2 = [{id, Id} | Proplist1],

    JSON = epl_json:encode(Proplist2, <<"system-info">>),

    [Pid ! {data, JSON} || Pid <- Subs],

    {noreply, State};
handle_info(Info, _State) ->
    exit({not_implemented, Info}).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

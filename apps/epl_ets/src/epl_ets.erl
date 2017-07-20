%%%-------------------------------------------------------------------
%% @doc epl_ets module.
%% It is a gen_server listening to messages from epl main application.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets).

-behaviour(gen_server).

%% API
-export([start_link/0,
         subscribe/0,
         subscribe/1,
         unsubscribe/0,
         unsubscribe/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Const
-define(ALIVE_TIMEOUT, 10000).
-define(CHECK_NODE_TIMER, 10000).

%% gen_server state
-record(state, {subscribers = [],
                vizceral = #{},
                nodes = #{}}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Stars epl_ets.
-spec start_link() -> {ok, Pid :: pid()} |
                      ignore |
                      {reason, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Add calling process to epl_ets subscribers.
-spec subscribe() -> ok.
subscribe() ->
    subscribe(self()).

%% @doc Add provided `Pid` to epl_ets subscribers.
-spec subscribe(Pid :: pid()) -> ok.
subscribe(Pid) ->
    gen_server:cast(?MODULE, {subscribe, Pid}).

%% @doc Remove calling process from epl_ets subscribers.
-spec unsubscribe() -> ok.
unsubscribe() ->
    unsubscribe(self()).

%% @doc Remove provided `Pid` from epl_ets subscribers.
-spec unsubscribe(Pid :: pid()) -> ok.
unsubscribe(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Res = epl:subscribe(),
    true = if_subs_ok(Res),
    check_node_timer(),
    Node = epl:get_default_node(),
    VizEntity = epl_viz_map:new(Node),
    {ok, #state{vizceral = VizEntity}}.

handle_call(Request, _From, _State) ->
    exit({not_implemented, Request}).

handle_cast({subscribe, Pid}, State = #state{subscribers = Subs}) ->
    NewSubs = [Pid | Subs],
    NewState = State#state{subscribers = NewSubs},
    {noreply, NewState};
handle_cast({unsubscribe, Pid}, State = #state{subscribers = Subs}) ->
    NewSubs = lists:delete(Pid, Subs),
    NewState = State#state{subscribers = NewSubs},
    {noreply, NewState}.

handle_info({data, {Node, _Timestamp}, Proplist},
            State = #state{subscribers = Subs, vizceral = VizEntity,
                          nodes = SubsNodes}) ->
    NewSubsNodes = register_node_activity(Node, SubsNodes),
    NewViz = epl_ets_viz_map:update_cluster(Node, VizEntity),
    NewViz2 = epl_ets_tab_map:update_node(Node, Proplist, NewViz),
    distribute_viz(NewViz2, Subs),
    NewState = State#state{vizceral = NewViz2, nodes = NewSubsNodes},
    {noreply, NewState};
handle_info(check_nodes_state, State = #state{vizceral = VizEntity,
                                                nodes = SubsNodes}) ->
    NewSubsNodes = remove_outdated(SubsNodes),
    NewVizEntity = epl_ets_viz_map:remove_outdated(NewSubsNodes, VizEntity),
    check_node_timer(),
    NewState = State#state{vizceral = NewVizEntity, nodes = NewSubsNodes},
    {noreply, NewState}.

terminate(_Reason, _State) ->
    epl:unsubscribe().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internals
%%====================================================================
    
distribute_viz(Viz, Subs) ->
    JSON = epl_json:encode(Viz, <<"ets-node-info">>),
    [Pid ! {data, JSON} || Pid <- Subs].

register_node_activity(Node, Nodes) ->
    maps:put(Node, erlang:system_time(millisecond), Nodes).

remove_outdated(Nodes) ->
    Now = erlang:system_time(millisecond),
    maps:filter(fun(_Node, Timestamp) ->
                                   Now - Timestamp < ?ALIVE_TIMEOUT end, Nodes).

check_node_timer() ->
    erlang:send_after(?CHECK_NODE_TIMER, self(), check_nodes_state).

if_subs_ok(Res) ->
    lists:all(fun(R) -> ok == R end, Res).

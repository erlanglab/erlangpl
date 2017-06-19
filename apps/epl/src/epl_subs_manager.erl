%%%-------------------------------------------------------------------
%% @doc epl_subs_manager module.
%% It is a gen_server listening to messages about nodes' state. Basing on
%% that information it enables/disables subscription to a particular node.
%% @end
%%%-------------------------------------------------------------------

-module(epl_subs_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
         enable_dynamic_sub/1,
         disable_dynamic_sub/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {subscribers = []}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Stars epl_subs_manager.
-spec start_link() -> {ok, Pid :: pid()} |
                      ignore |
                      {reason, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Dynamically makes provided `Pid' subscribe to every newly connected
%% node.
-spec enable_dynamic_sub(Pid :: pid()) -> ok.
enable_dynamic_sub(Pid) ->
    gen_server:cast(?MODULE, {add_sub, Pid}).

%% @doc Disables dynamic subscription for provided `Pid'.
-spec disable_dynamic_sub(Pid :: pid()) -> ok.
disable_dynamic_sub(Pid) ->
    gen_server:cast(?MODULE, {del_sub, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ok = net_kernel:monitor_nodes(true),
    {ok, #state{}}.

handle_call(Request, _From, _State) ->
    exit(not_implemented, Request).

handle_cast({add_sub, Pid}, State = #state{subscribers = Subs}) ->
    NewSubs = [Pid | Subs],
    NewState = State#state{subscribers = NewSubs},
    {noreply, NewState};
handle_cast({del_sub, Pid}, State = #state{subscribers = Subs}) ->
    NewSubs = lists:delete(Pid, Subs),
    NewState = State#state{subscribers = NewSubs},
    {noreply, NewState}.

handle_info({nodeup, Node}, State = #state{subscribers = Subs}) ->
    {ok, _} = epl_tracer_sup:start_child([Node]),
    [epl:subscribe(Node, Pid) || Pid <- Subs],
    {noreply, State};
handle_info({nodedown, _Node}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

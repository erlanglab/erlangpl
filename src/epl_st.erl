%%%-------------------------------------------------------------------
%%% @doc
%%% gen_sever listening to events from epl_tracer and calculating
%%% supervision trees
%%% @end
%%%-------------------------------------------------------------------
-module(epl_st).

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

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe() ->
    gen_server:cast(?MODULE, {subscribe, self()}).

unsubscribe() ->
    gen_server:cast(?MODULE, {unsubscribe, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = epl:subscribe(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({subscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = [Pid|Subs]}};
handle_cast({unsubscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({data, _, _}, State = #state{subscribers = Subs}) ->
    AppsInfo = command(fun application:info/0),
    Apps = lists:map(fun({Name, Pid}) -> {Name, generate_sup_tree(Pid)} end,
                     proplists:get_value(running, AppsInfo)),
    JSON = encode([{applications, Apps}]),

    [Pid ! {data, JSON} || Pid <- Subs],
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_sup_tree({_Name, Pid, worker, _Mods}) ->
    worker_node(Pid);
generate_sup_tree({_Name, Pid, supervisor, _Mods}) ->
    Children = command(fun supervisor:which_children/1, [Pid]),
    sup_node(Pid, lists:map(fun generate_sup_tree/1, Children));
generate_sup_tree(undefined) ->
    [];
generate_sup_tree(MasterPid) ->
    {SupPid, _SupName} = command(fun application_master:get_child/1, [MasterPid]),
    Children = command(fun supervisor:which_children/1, [SupPid]),
    sup_node(SupPid, lists:map(fun generate_sup_tree/1, Children)).

worker_node(Pid) ->
    tree_node(Pid, worker, []).

sup_node(Pid, Children) ->
    tree_node(Pid, supervisor, Children).

tree_node(Pid, Type, Children) ->
    [{id, epl:to_bin(Pid)}, {type, Type}, {children, Children}].

encode(Proplist) ->
    ej:encode(Proplist).

command(Fun) ->
    command(Fun, []).

command(Fun, Args) ->
    {ok, Result} = epl_tracer:command(Fun, Args),
    Result.

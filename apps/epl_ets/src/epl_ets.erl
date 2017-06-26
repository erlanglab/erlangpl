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

handle_info({data, {Node, _Timestamp}, _Proplist},
            State = #state{subscribers = Subs, vizceral = VizEntity,
                          nodes = SubsNodes}) ->
    NewSubsNodes = register_node_activity(Node, SubsNodes),
    NewViz = update_viz(Node, VizEntity, SubsNodes),
    distribute_viz(NewViz, Subs),
    NewState = State#state{vizceral = NewViz, nodes = NewSubsNodes},
    {noreply, NewState};
handle_info(check_nodes_state, State = #state{vizceral = VizEntity,
                                                nodes = SubsNodes}) ->
    NewSubsNodes = remove_outdated(SubsNodes),
    NewVizEntity = remove_outdated(NewSubsNodes, VizEntity),
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

get_ets_basic_info(Node) ->
    ETSCount = get_all_ets_count(Node),
    ETSMemUsage = get_ets_mem_usage(Node),
    ETSPieChart = get_ets_pie_chart(ETSMemUsage, 0, 0),
    #{etsMetrics => 
          #{all => ETSCount, memUsage => ETSMemUsage, pieChart => ETSPieChart}}.

get_all_ets_count(Node) ->
    {ok, AllETS} = epl:command(Node, fun ets:all/0, []),
    erlang:length(AllETS).

get_ets_mem_usage(Node) ->
    {ok, MemoryData} = epl:command(Node, fun erlang:memory/0, []),
    MemoryPercent = proplists:get_value(ets, MemoryData) / 
        proplists:get_value(total, MemoryData),
    trunc_float(MemoryPercent, 4).
    
update_viz(Node, Viz, Nodes) ->
    VizRegion = push_unique_region(Node, Viz, maps:is_key(Node, Nodes)),
    ETSBasicInfo = get_ets_basic_info(Node),
    epl_viz_map:push_additional_node_info(ETSBasicInfo, Node, VizRegion).

distribute_viz(Viz, Subs) ->
    JSON = epl_json:encode(Viz, <<"ets-node-info">>),
    [Pid ! {data, JSON} || Pid <- Subs].

push_unique_region(Node, Viz, false) ->
    VizRegion = epl_viz_map:push_region(Node, Viz),
    push_fake_conn(Node, VizRegion);
push_unique_region(_Node, Viz, true) ->
    Viz.

push_fake_conn(Node, Viz) ->
    epl_viz_map:push_connection(epl:get_default_node(), Node, {0, 0, 0}, #{}, 
                                Viz).

register_node_activity(Node, Nodes) ->
    maps:put(Node, erlang:system_time(millisecond), Nodes).

remove_outdated(Nodes) ->
    Now = erlang:system_time(millisecond),
    maps:filter(fun(_Node, Timestamp) ->
                                   Now - Timestamp < ?ALIVE_TIMEOUT end, Nodes).

remove_outdated(Nodes, Viz) ->
    NodesList = maps:keys(Nodes),
    VizNodes = [pull_node(N, Viz) || N <- NodesList],
    VizConn = [pull_fake_conn(N, Viz) || N <- NodesList],
    NewViz = maps:merge(Viz, #{nodes => VizNodes}),
    maps:merge(NewViz, #{connections => VizConn}).

pull_node(Node, Viz) ->
    {N, _Rest} = epl_viz_map:pull_node(Node, Viz),
    N.

pull_fake_conn(Node, #{connections := Conns}) ->
    [Conn] = lists:filter(fun(Elem) -> maps:get(target, Elem) == 
                                           epl_viz_map:namify(Node) end, Conns),
    Conn.

check_node_timer() ->
    erlang:send_after(?CHECK_NODE_TIMER, self(), check_nodes_state).

if_subs_ok(Res) ->
    lists:all(fun(R) -> ok == R end, Res).

trunc_float(Float, Pos) ->
    List = erlang:float_to_list(Float, [{decimals, Pos}]),
    erlang:list_to_float(List).

get_ets_pie_chart(N, D, W) ->
    #{normal => N, danger => D, warning => W}.

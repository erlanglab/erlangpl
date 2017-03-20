%%% Copyright (c) 2017, erlang.pl
%%%-------------------------------------------------------------------
%%% @doc
%%% gen_server listening to events from epl_tracer
%%% and constructing a #map{} representation of a graph,
%%% where verticies represent Erlang nodes or processes,
%%% and edges represent inter-node traffic or message passing.
%%% @end
%%%-------------------------------------------------------------------
-module(epl_traffic).

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

-record(state, {subscribers = [],
                counters = []}).

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
    %% Subscribe to all events from the observed node
    ok = epl:subscribe(),

    %% Initialise counters, so that later we can calculate deltas
    Counters = get_traffic_counters(),
    {ok, #state{counters = Counters}}.

handle_call(Request, _From, _State) ->
    exit({not_implemented, Request}).

handle_cast({subscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = [Pid|Subs]}};
handle_cast({unsubscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({data, _, _}, State = #state{subscribers = Subs,
                                         counters = OldCounters}) ->
    %% traverse all connected nodes and read their net_kernel counters
    NewCounters = get_traffic_counters(),

    %% start creating a map, which represents the Vizceral JSON document
    %% region named "INTERNET" is mandatory
    V1 = push_region(<<"INTERNET">>, new()),

    Nodes = nodes(connected),
    %% add as many regions as there are nodes in the cluster
    V2 = lists:foldl(fun(Node, V) ->
                             NodeBin = atom_to_binary(Node, latin1),
                             push_region(NodeBin, V)
                     end, V1, Nodes),

    %% add links between "INTERNET" and all nodes
    %% and compute delta between old and new net_kernel counters
    V3 = lists:foldl(fun(Node, V) ->
                             NodeBin = atom_to_binary(Node, latin1),
                             {OldIn, _OldOut} = get_in_out(Node, OldCounters),
                             {NewIn, _NewOut} = get_in_out(Node, NewCounters),
                             push_region_connection(
                               <<"INTERNET">>, NodeBin, {NewIn-OldIn, 0, 0}, #{}, V)
                     end, V2, Nodes),

    %% push an update to all subscribed WebSockets
    JSON = epl_json:encode(V3, <<"traffic-info">>),

    [Pid ! {data, JSON} || Pid <- Subs],
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_traffic_counters() ->
    {ok, NodesInfo} = command(fun net_kernel:nodes_info/0),

    %% map results of calling net_kernel:nodes_info/0
    %% to a list of 3-element tuples {Node, Input, Output}
    [{NodeName, NodeIn, NodeOut} ||
        {NodeName, [{owner,_}, {state,up}, {address, _}, {type,normal},
                    {in,NodeIn}, {out,NodeOut}]} <- NodesInfo].

get_in_out(Node, Counters) ->
    case lists:keyfind(Node, 1, Counters) of
        {Node, In, Out} ->
            {In, Out};
        false ->
            {0, 0}
    end.

command(Fun) ->
    command(Fun, []).

command(Fun, Args) ->
    {ok, Result} = epl_tracer:command(Fun, Args),
    Result.

%%%===================================================================
%%% functions manipulating Vizceral map
%%%===================================================================
new() ->
    entity(global, "edge", [], #{connections => []}).

entity(Renderer, Name, Nodes, Additional) ->
    Map = #{
      renderer => Renderer,
      name => namify(Name),
      displayName => binarify(Name),
      nodes => Nodes
     },
    maps:merge(Map, Additional).

binarify(Name) when is_list(Name) ->
    list_to_binary(Name);
binarify(Name) -> Name.

namify(Name) ->
    Name1 = binary:replace(binarify(Name), <<"@">>, <<"0">>),
    Name2 = binary:replace(binarify(Name1), <<"<">>, <<"">>),
    Name3 = binary:replace(binarify(Name2), <<">">>, <<"">>),
    binary:replace(binarify(Name3), <<".">>, <<"-">>, [global]).


%% ---------------------- Regions ---------------------
push_region(Name, Vizcerl) ->
    push_region(Name, #{}, Vizcerl).
push_region(Name, Additional, Vizcerl) ->
    A = maps:merge(#{
                      connections => [],
                      maxVolume => 5000
                    },
                   Additional),
    push_node(region, Name, A, Vizcerl).

pull_region(Name, Vizcerl) ->
    {Region, Newlist} = pull_node(Name, Vizcerl),
    {Region, maps:merge(Vizcerl, #{nodes => Newlist})}.

%% ---------------------- Nodes -----------------------
push_node(Renderer, Name, Additional, Entity) ->
    #{nodes := Nodes} = Entity,
    Newnode = entity(Renderer, Name, [], Additional),
    maps:merge(Entity, #{nodes => [Newnode | Nodes]}).
pull_node(Name, Entity) ->
    #{nodes := Nodes} = Entity,
    {[Node], Rest} = lists:partition(
                       fun(A) ->
                               maps:get(name, A) == namify(Name)
                       end, Nodes),
    {Node, Rest}.

%% ------------------- Connections --------------------
push_connection(Source, Target, {N, W, D} , Additional, To) ->
  #{connections := Connections} = To,
  New = maps:merge(#{
    source => namify(Source),
    target => namify(Target),
    metrics => #{
      normal => N,
      danger => D,
      warning => W
     }
   }, Additional),
  maps:merge(To, #{connections => [New | Connections]}).

push_region_connection(Source, Target, {N, W, D}, Additional, Vizcerl) ->
  %% Will crash on nonexisting
  pull_region(Source, Vizcerl),
  pull_region(Target, Vizcerl),
  push_connection(Source, Target, {N, W, D}, Additional, Vizcerl).

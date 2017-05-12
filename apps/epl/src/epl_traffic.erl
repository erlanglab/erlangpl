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
                traffic = [],
                msg_pass = #{},
                cluster_traffic = []}).

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
    ok = epl:subscribe(default_node),

    %% Initialise counters, so that later we can calculate deltas
    TrafficCounters = get_traffic_counters(),
    {ok, #state{traffic = TrafficCounters}}.

handle_call(Request, _From, _State) ->
    exit({not_implemented, Request}).

handle_cast({subscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = [Pid|Subs]}};
handle_cast({unsubscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};
handle_cast(Request, _State) ->
    exit({not_implemented, Request}).

handle_info({data, {Node, _Timestamp}, _Proplist},
            State = #state{subscribers = Subs,
                           traffic = OldTraffic,
                           msg_pass = OldMsgPass,
                           cluster_traffic = OldClusterTraffic}) ->

    {Viz1, NewTraffic} = update_traffic_graph(Node, OldTraffic,
                                              epl_viz_map:new(Node)),

    NewClusterTraffic = get_cluster_traffic_counters(nodes()),
    TrafficDelta = compute_delta(NewClusterTraffic, OldClusterTraffic),


    Proplist = [{send, TrafficDelta}],
    io:format("Old Traffic ~p~n~n", [OldClusterTraffic]),
    io:format("New Delta ~p~n~n", [TrafficDelta]),

    %% We're starting from observed node which is our graph entry point
    Viz2 = get_message_passing_counters(Node, Proplist,
                                        Viz1, OldMsgPass),

    %% push an update to all subscribed WebSockets
    JSON = epl_json:encode(Viz2, <<"traffic-info">>),

    [Pid ! {data, JSON} || Pid <- Subs],
    {noreply, State#state{traffic = NewTraffic,
                          cluster_traffic = NewClusterTraffic}};
handle_info(Request, _State) ->
    exit({not_implemented, Request}).

compute_delta(NewClusterTraffic, OldClusterTraffic) ->
    lists:map(
      fun({Key, In, Out}) ->
              case lists:keyfind(Key, 1, OldClusterTraffic) of
                  {_, OldIn, OldOut} -> {Key, In-OldIn, Out-OldOut};
                  false -> {Key, 0, 0}
              end
      end,
      NewClusterTraffic).

get_cluster_traffic_counters(Nodes) ->
    NodesInfo = [{Node, element(2, rpc:call(Node, net_kernel, nodes_info, []))}
                 || Node <- Nodes],

    %% Example of NodesInfo:
    %% {'michal@127.0.0.1',
    %%   [{'bartosz@127.0.0.1',
    %%         [{owner,<13641.75.0>},
    %%          {state,up},
    %%          {address,
    %%           {net_address,
    %%            {{127,0,0,1},49358},
    %%            "127.0.0.1",tcp,inet}},
    %%          {type,normal},
    %%          {in,38},
    %%          {out,38}]},
    %%        {'erlangpl@127.0.0.1',
    %%         [{owner,<13641.144.0>},
    %%          {state,up},
    %%          {address,
    %%           {net_address,
    %%            {{127,0,0,1},49404},
    %%            "127.0.0.1",tcp,inet}},
    %%          {type,normal},
    %%          {in,32},
    %%          {out,22}]}]}
    lists:flatten(
      lists:map(fun({NodeA, NeighboursInfo}) ->
                        [{{NodeA, NodeB}, NodeIn, NodeOut} ||
                            {NodeB, [_, _, _, _,{in,NodeIn}, {out,NodeOut}]}
                                <- NeighboursInfo]
                end, NodesInfo)).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_message_passing_counters(Node, Proplist, Vizceral, OldMsgPass) ->
    lists:foldl(
      fun({send, Send}, V) ->
              %% Examples of send trace:
              %% {{global_name_server,<13104.13.0>},0,1}
              %% {{#Port<13104.431>,<13104.28.0>},0,72}
              %% {{<13104.12.0>,{alias,'erlangpl@127.0.0.1'}},2,0}
              update_message_passing_graph(Node, Send, V, OldMsgPass);
         (_, V) ->
              V
      end,
      Vizceral,
      Proplist).

update_message_passing_graph(Node, Send, Vizceral, _OldMsgPass) ->
    %% the INTERNET node represents the source of ingress traffic
    Vizceral1 = push_focused(<<"INTERNET">>, Node, Vizceral),

    lists:foldl(
      fun({{ID1, ID2}, Count1, Count2}, V) ->
              P1 = get_PID_from_trace_event(ID1),
              P2 = get_PID_from_trace_event(ID2),
              update_messge_passing_graph(Node, P1, P2, Count1, Count2, V)
      end, Vizceral1, Send).

get_PID_from_trace_event({P,_}) -> P;
get_PID_from_trace_event(P)     -> P.

update_messge_passing_graph(Node, P1, P2, Count1, Count2, V) ->
    %% Add verticies representing processes and ports
    %% and edges representing message passing
    V1 = update_msg_pass_processes(Node, P1, P2, Count1, Count2, V),

    %% Add edges between <<"INTERNET">> and ports
    %% to represent ingress traffic
    update_msg_pass_ports(Node, P1, P2, Count1, Count2, V1).

update_msg_pass_processes(Node, P1, P2, C1, C2, V) ->
    V1 = push_focused(P1, Node, V),
    V2 = push_focused(P2, Node, V1),
    V3 = push_focused_connection(P1, P2, Node, {C1,0,0}, V2),
    push_focused_connection(P2, P1, Node, {C2,0,0}, V3).

update_msg_pass_ports(Node, P1, P2, C1, C2, V) ->
    V1 = if is_port(P1) ->
                 push_focused_connection(<<"INTERNET">>, P1,
                                         Node, {C1,0,0}, V);
            true ->
                 V
         end,

    if is_port(P2) ->
            push_focused_connection(<<"INTERNET">>, P2,
                                    Node, {C2,0,0}, V1);
       true ->
            V1
    end.

get_traffic_counters() ->
    {ok, NodesInfo} = command(fun net_kernel:nodes_info/0),

    %% map results of calling net_kernel:nodes_info/0
    %% to a list of 3-element tuples {Node, Input, Output}
    [{NodeName, NodeIn, NodeOut} ||
        {NodeName, [{owner,_}, {state,up}, {address, _}, {type,normal},
                    {in,NodeIn}, {out,NodeOut}]} <- NodesInfo].

update_traffic_graph(EntryNode, OldCounters, Vizceral) ->
    %% traverse all connected nodes and read their net_kernel counters
    NewCounters = get_traffic_counters(),

    %% start creating a map, which represents the Vizceral JSON document
    %% region named <<"INTERNET">> represents the observed node
    V1 = epl_viz_map:push_region(EntryNode, Vizceral),

    %% add as many regions as there are nodes in the cluster
    V2 = lists:foldl(fun({Node,_,_}, V) ->
                             epl_viz_map:push_region(epl_viz_map:binarify(Node),
                                                     V)
                     end, V1, NewCounters),

    %% add links between "INTERNET" and all nodes
    %% and compute delta between old and new net_kernel counters
    V3 = lists:foldl(
           fun({Node, NewIn, NewOut}, V) ->
                   {OldIn, OldOut} = get_in_out(Node, OldCounters),
                   push_region_connection(EntryNode, epl_viz_map:binarify(Node),
                                          {NewOut-OldOut, NewIn-OldIn, 0},
                                          #{}, V)
           end, V2, NewCounters),

    {V3, NewCounters}.

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
    {ok, Result} = epl:command(default_node, Fun, Args),
    Result.

%%%===================================================================
%%% functions manipulating Vizceral map
%%%===================================================================

%% ------------------- Connections --------------------
push_region_connection(Source, Target, {N, W, D}, Additional, Vizceral) ->
    %% Will crash on nonexisting
    epl_viz_map:pull_region(Source, Vizceral),
    epl_viz_map:pull_region(Target, Vizceral),
    %% Outgoing traffic
    Viz = epl_viz_map:push_connection(Source, Target, {N, 0, D}, Additional, Vizceral),
    %% Incoming  traffic
    epl_viz_map:push_connection(Target, Source, {W, 0, D}, Additional, Viz).

push_focused_connection(S, T, RN, NWD, Vizceral) ->
    push_focused_connection(S, T, RN, NWD, #{}, Vizceral).

push_focused_connection(Source, Target, RegionName, {N, W, D}, A, Vizceral) ->
    {Region, NewV} = epl_viz_map:pull_region(RegionName, Vizceral),
    NewR = epl_viz_map:push_connection(Source, Target, {N,W,D}, A, Region),
    epl_viz_map:push_region(RegionName, NewR, NewV).

%% ---------------------- Focused ---------------------
push_focused(Name, Region, Vizceral) ->
    push_focused(Name, Region, #{} ,Vizceral).

push_focused(Name, RegionName, Additional, Vizceral) ->
    #{nodes := Nodes} = Vizceral,
    {[Region], Rest} = lists:partition(
                         fun(A) ->
                                 maps:get(name, A) 
                                     == epl_viz_map:namify(RegionName)
                         end, Nodes),
    NewRegion = epl_viz_map:push_node(focusedChild, Name, Additional, Region),
    maps:merge(Vizceral, #{nodes => [NewRegion | Rest]}).

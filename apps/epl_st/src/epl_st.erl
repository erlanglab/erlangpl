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
         unsubscribe/0,
         node_info/1]).

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

node_info(NodeId) when is_list(NodeId) ->
    Info =
        case node_id_to_pid(NodeId) of
            {ok, Pid} ->
                get_process_info(Pid);
            error ->
                #{error => <<"Ports are not supported yet">>}
        end,
    epl_json:encode(#{id => list_to_binary(NodeId), info => Info}, <<"node-info">>);
node_info(NodeId) when is_binary(NodeId) ->
    node_info(binary_to_list(NodeId)).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = epl:subscribe(default_node),
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
    Apps = lists:foldl(fun({Name, Pid}, Acc) ->
                               maps:put(Name, generate_sup_tree(Pid), Acc)
                       end,
                       #{},
                       proplists:get_value(running, AppsInfo)),
    JSON = epl_json:encode(Apps, <<"apps-info">>),

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
    sup_node(Pid, generate_children(Children));
generate_sup_tree(undefined) ->
    #{};
generate_sup_tree(MasterPid) ->
    {SupPid, _SupName} = command(fun application_master:get_child/1, [MasterPid]),
    Children = command(fun supervisor:which_children/1, [SupPid]),
    sup_node(SupPid, generate_children(Children)).

generate_children(Children) ->
    lists:filtermap(
      fun({_, undefined, _, _}) ->
              false;
         (Child) ->
              {true, generate_sup_tree(Child)}
      end,
      Children).

worker_node(Pid) ->
    tree_node(Pid, worker, []).

sup_node(Pid, Children) ->
    tree_node(Pid, supervisor, Children).

tree_node(Pid, Type, Children) ->
    #{id => epl:to_bin(Pid), type => Type, children => Children}.

command(Fun) ->
    command(Fun, []).

command(Fun, Args) ->
    {ok, Result} = epl_tracer:command(default_node, Fun, Args),
    Result.

node_id_to_pid(NodeId) when is_list(NodeId) ->
    case catch list_to_pid(NodeId) of
        {'EXIT',{badarg,_}} ->
            error;
        Pid ->
            {ok, Pid}
    end.

get_process_info(Pid) ->
    case epl:process_info(Pid) of
        {ok, undefined} ->
            #{status => exited};
        {ok, ProcessInfo} ->
            format_process_info(ProcessInfo)
    end.

format_process_info(ProcessInfo) ->
    lists:foldl(fun ({K, V}, Acc) ->
                        FormattedVal = format_process_info(K, V),
                        maps:put(K, FormattedVal, Acc)
                end, #{}, ProcessInfo).

format_process_info(current_function, MFA) ->
    format_mfa(MFA);
format_process_info(initial_call, MFA) ->
    format_mfa(MFA);
format_process_info(dictionary, Dict) ->
    lists:foldl(fun ({K, V}, Acc) ->
                        FormattedVal = format_dictionary_item(K, V),
                        maps:put(K, FormattedVal, Acc)
                end, #{}, Dict);
format_process_info(_K, V) ->
    list_to_binary(io_lib:format("~p", [V])).

format_dictionary_item('$initial_call', MFA) ->
    format_mfa(MFA);
format_dictionary_item('$ancestors', V) ->
    V;
format_dictionary_item(_K, V) ->
    list_to_binary(io_lib:format("~p", [V])).

format_mfa({M, F, A}) ->
    #{<<"module">> => M, <<"function">> => F, <<"arity">> => A}.

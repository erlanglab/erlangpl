%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Parse and return command line arguments
    Args = run1(),
    %% TODO: add following debug printouts, if verbosity flag set
    %% io:format("Plain args: ~p~n", [init:get_plain_arguments()]),
    %% io:format("Args: ~p~n", [Args]),

    %% Check if 'node' argument has been passed
    Node = run2(Args),

    %% Connect to remote node and get its OS pid
    NodeSettings = run3(Node, Args),

    %% settings and static files are kept in epl_priv ets table
    ets:new(epl_priv, [named_table, {read_concurrency,true}]),
    ets:insert(epl_priv, {node, Node}),
    ets:insert(epl_priv, {node_settings, NodeSettings}),

    %% Load priv files to ets
    ok = run4(),

    %% Start top supervisor
    {ok, Pid} = epl_sup:start_link(),

    %% add all EPL handlers to the supervision tree
    run5(),

    %% return top supervisor Pid to application controller
    {ok, Pid}.

stop(_State) ->
    ok.

%% ===================================================================
%% internal functions
%% ===================================================================

%% Parse and return command line arguments
run1() ->
    %% get arguments passed to erl from command line
    A = init:get_plain_arguments(),

    %% parse arguments and display help if no valid arguments
    case getopt:parse(option_spec_list(), A) of
        {ok, {[], []}} ->
            help();
        {ok, {Options, NonOptArgs}} ->
            Args = filter_flags(NonOptArgs, []),
            Options ++ Args;
        {error, {Reason, Data}} ->
            log(error, "~s ~p", [Reason, Data]),
            help()
    end.

%% Check if node argument has been passed
run2(Args) ->
    case proplists:lookup(node, Args) of
        none ->
            log(error, "provide node name, e.g. --node mynode@127.0.0.1"),
            help();
        {node, NodeStr} ->
            list_to_atom(NodeStr)
    end.

%% Connect to remote node and get its OS pid
run3(Node, Args) ->
    {ok, _Pid} = start_distributed(Args),

    true = setcookie(Node, Args),

    true = connect_node(Node),

    Flags = [allocated_areas,
             build_type,
             port_limit,
             garbage_collection,
             kernel_poll,
             otp_release,
             process_limit,
             schedulers,
             smp_support,
             system_version,
             threads,
             thread_pool_size,
             version,
             wordsize
            ],
    SystemInfo = [{Flag, fun erlang:system_info/1, [Flag]} || Flag <- Flags],

    GetSettingsList = [{node_pid, fun os:getpid/0, []},
                       {memory, fun erlang:memory/0, []}
                       | SystemInfo],

    RemoteFunStr =
        "fun () ->
                receive
                    {Ref, Pid, List} ->
                        NodeSettings = [{Key, catch apply(Fun, Args)}
                                        || {Key, Fun, Args} <- List],
                        Pid ! {Ref, NodeSettings}
                after 1000 ->
                        ok
                end
        end.",
    {ok, Tokens, _} = erl_scan:string(RemoteFunStr),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    {value, RemoteFun, _} = rpc:call(Node, erl_eval, expr, [Form, []]),

    Ref = make_ref(),
    RemotePid = spawn(Node, erlang, apply, [RemoteFun, []]),
    RemotePid ! {Ref, self(), GetSettingsList},

    receive
        {Ref, NodeSettings} when is_list(NodeSettings) ->
            %% TODO: shall we disconnect from it for security?
            %% net_kernel:disconnect(Node),
            NodeSettings
    after 5000 ->
            log(error, "timed out when collecting settings"
                " of remote node ~p", [Node]),
            halt(1, [{flush, true}])
    end.

%% Load priv files to ets
run4() ->
    %% check if we run from escript or regular release
    %% if the latter, load private files from local file system
    {ok, PrivFiles} = case init:get_argument(progname) of
                          {ok,[["erl"]]} ->
                              escript_foldl();
                          {ok,[["erlangpl"]]} ->
                              filesystem_foldl()
                      end,
    load_files_into_ets(PrivFiles).

run5() ->
    %% Find modules that end with _EPL
    %% by convention such modules implement EPL handlers
    LoadedModules = code:all_loaded(),
    GetHandlers = fun({_,preloaded}, Acc) ->
                          Acc;
                     ({Mod, Path}, Acc) ->
                          case re:run(Path,"_EPL.beam\$",[global]) of
                              {match,_} -> [Mod | Acc];
                              nomatch   -> Acc
                          end
                  end,
    HandlerModules = lists:foldl(GetHandlers, [], LoadedModules),
    _Pids = [{ok, _} = epl_sup:start_child(M)
             || M <- HandlerModules],

    %% TODO: add following debug printouts, if verbosity flag set
    %% io:format("Mod: ~p Pids: ~p~n", [HandlerModules, Pids]),
    CowboyModules =
        lists:flatten(
          [[{"/"++atom_to_list(Mod), Mod, []},
            {"/"++atom_to_list(Mod)++"/[...]", epl_static, Mod}]
           || Mod <- HandlerModules]),
    Dispatch = cowboy_router:compile(
                 [{'_', CowboyModules ++
                       [{"/[...]", epl_static, epl}]}
                 ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8000}],
                                [{env, [{dispatch, Dispatch}]}]).

help() ->
    getopt:usage(option_spec_list(), "erlangpl", [], []),
    halt(1, [{flush, true}]).

option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {node,    $n, "node",      string,    "Monitored node name"},
     {cookie,  $c, "cookie",    string,    "Overwrite ~/.erlang.cookie"},
     {help,    $h, "help",      undefined, "Show the program options"},
     {verbose, $v, "verbose",   integer,   "Verbosity level (-v, -vv, -vvv)"},
     {version, $V, "version",   undefined, "Show version information"},
     {sname,   $s, "sname",     string,    "Start with a shortname"},
     {name,    $l, "name",      string,    "Start with a longname, default "
                                           "erlangpl@127.0.0.1"}
    ].

filter_flags([], Commands) ->
    lists:reverse(Commands);
filter_flags([Item | Rest], Commands) ->
    case string:tokens(Item, "=") of
        [Command] ->
            filter_flags(Rest, [Command | Commands]);
        [KeyStr, Value] ->
            Key = list_to_atom(KeyStr),
            filter_flags(Rest, [{Key, Value}|Commands]);
        Other ->
            log(warning, "Ignoring command line argument: ~p\n", [Other]),
            filter_flags(Rest, Commands)
    end.

%% get file contents from escript
escript_foldl() ->
    File = filename:absname(escript:script_name()),
    case escript:extract(File, [compile_source]) of
        {ok, [_Shebang, _Comment, _EmuArgs, {archive, ArchiveBin}]} ->
            Fun = fun(Name, _, GetBin, Acc) ->
                          [{list_to_binary(Name), GetBin()} | Acc]
                  end,
            zip:foldl(Fun, [], {File, ArchiveBin});
        {error, _} = Error ->
            Error
    end.

filesystem_foldl() ->
    Paths = filelib:wildcard("lib/*"),
    filesystem_foldl(Paths).

%% get file contents from file system
filesystem_foldl(Paths) ->
    Fun = fun(Path, Acc) ->
                  %% drop "lib/" prefix
                  SplitPath = tl(filename:split(Path)),
                  %% remove application version number
                  AppDir = hd(SplitPath),
                  AppName = hd(string:tokens(AppDir, "-")),
                  %% re-create file path
                  Filename = filename:join([AppName | tl(SplitPath)]),
                  [{list_to_binary(Filename), file_contents(Path)} | Acc]
          end,

    {ok, lists:flatten(
           [filelib:fold_files(filename:join([Dir, "priv"]), "", true, Fun, [])
            || Dir <- Paths])}.

file_contents(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Bin.

load_files_into_ets(Files) ->
    Fun = fun({File, Bin}) ->
                  case filename:extension(File) of
                      Skip when Skip == <<".beam">>; Skip == <<".app">> ->
                          ok;
                      _ ->
                          %% TODO: add following debug printouts
                          %% io:format("~p~n", [File]),
                          true = ets:insert(epl_priv, {File, Bin})
                  end
          end,
    lists:foreach(Fun, Files).

log(error, Str) ->
    log(error, Str, []);
log(warning, Str) ->
    log(warning, Str, []).

log(error, Str, List) ->
    io:format(lists:flatten(["ERROR: ", Str, "~n~n"]), List);
log(warning, Str, List) ->
    io:format(lists:flatten(["WARNING: ", Str, "~n~n"]), List).

start_distributed(Args) ->
    application:set_env(kernel, dist_auto_connect, never),

    %% By default we start as -name erlangpl@127.0.0.1
    %% if --sname or --name flag was passed, we start accordingly
    R = case {proplists:lookup(name, Args), proplists:lookup(sname, Args)} of
            {none, none} ->
                net_kernel:start(['erlangpl@127.0.0.1', longnames]);
            {{name, Name}, _} ->
                net_kernel:start([list_to_atom(Name), longnames]);
            {_, {sname, SName}} ->
                net_kernel:start([list_to_atom(SName), shortnames])
        end,

    case R of
        {ok, Pid} ->
            {ok, Pid};
        {error,{{already_started, Pid}, _}} ->
            %% apparently the node is already alive
            {ok, Pid};
        Error ->
            log(error, "failed to start Erlang distributed ~p", [Error]),
            halt(1, [{flush, true}])
    end.

setcookie(Node, Args) ->
    case proplists:lookup(cookie, Args) of
        none ->
            true;
        {cookie, Cookie} ->
            erlang:set_cookie(Node, list_to_atom(Cookie))
    end.

connect_node(Node) ->
    case net_kernel:connect_node(Node) of
        true ->
            ok = net_kernel:allow([node()]),
            true;
        false ->
            log(error, "failed to connect to remote node ~p", [Node]),
            halt(1, [{flush, true}])
    end.

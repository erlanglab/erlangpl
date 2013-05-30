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

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), log(debug, Str, Args)).
-define(INFO(Str, Args), log(info, Str, Args)).
-define(WARN(Str, Args), log(warn, Str, Args)).
-define(ERROR(Str, Args), log(error, Str, Args)).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Parse and return command line arguments
    Args = run1(),

    %% epl_priv ets table is used to store all epl settings
    ets:new(epl_priv, [named_table, {read_concurrency,true}]),

    log_level(Args),

    show_version(Args),

    ?DEBUG("Plain args: ~p~n", [init:get_plain_arguments()]),
    ?DEBUG("Args: ~p~n", [Args]),

    %% Check if 'node' argument has been passed
    Node = run2(Args),

    %% Connect to remote node and get its OS pid
    NodeSettings = run3(Node, Args),

    %% settings and static files are kept in epl_priv ets table
    ets:insert(epl_priv, {node, Node}),
    ets:insert(epl_priv, {node_settings, NodeSettings}),

    %% Load priv files to ets
    ok = run4(),

    %% Start top supervisor
    {ok, Pid} = epl_sup:start_link(),

    %% load plugins
    plugins(Args),

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
            ?ERROR("~s ~p", [Reason, Data]),
            help()
    end.

%% Check if node argument has been passed
run2(Args) ->
    case proplists:lookup(node, Args) of
        none ->
            ?ERROR("provide node name, e.g. --node mynode@127.0.0.1", []),
            halt(1, [{flush, true}]);
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
            ?ERROR("timed out when collecting settings"
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
    Pids = [{ok, _} = epl_sup:start_child(M, [ets:lookup(epl_priv, node)])
            || M <- HandlerModules],

    ?INFO("Mod: ~p Pids: ~p~n", [HandlerModules, Pids]),

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
     {plugin,  $p, "plugin",    string,    "Path to plugins"},
     {help,    $h, "help",      undefined, "Show the program options"},
     {verbose, $v, "verbose",   integer,   "Verbosity level (-v, -vv, -vvv)"},
     {version, $V, "version",   undefined, "Show version information"},
     {sname,   $s, "sname",     string,    "Start with a shortname"},
     {name,    $l, "name",      string,    "Start with a longname, default "
                                           "erlangpl@127.0.0.1"}
    ].

%% show version information and halt
show_version(Args) ->
    case lists:member(version, Args) of
        false ->
            ok;
        true ->
            {ok, Vsn} = application:get_key(epl, vsn),
            ?CONSOLE("Erlang Performance Lab ~s\n", [Vsn])
    end.

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
            ?WARN("Ignoring command line argument: ~p\n", [Other]),
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
                          ?DEBUG("Loading: ~s~n", [File]),
                          true = ets:insert(epl_priv, {File, Bin})
                  end
          end,
    lists:foreach(Fun, Files).

log(Level, Str, Args) ->
    [{log_level, LogLevel}] = ets:lookup(epl_priv, log_level),
    case should_log(LogLevel, Level) of
        true ->
            io:format(log_prefix(Level) ++ Str, Args);
        false ->
            ok
    end.

%% set log level based on getopt option
log_level(Options) ->
    Verbose = case proplists:get_all_values(verbose, Options) of
                  [] ->
                      0;
                  Verbosities ->
                      lists:last(Verbosities)
              end,
    set_log_level(Verbose).

set_log_level(Verbose) when is_list(Verbose) ->
    set_log_level(list_to_integer(Verbose));
set_log_level(Verbose) when is_integer(Verbose) ->
    LogLevel = case Verbose of
                   0 -> error;
                   1 -> warn;
                   2 -> info;
                   3 -> debug
               end,

    true = ets:insert(epl_priv, {log_level, LogLevel}).

should_log(debug, _)     -> true;
should_log(info, debug)  -> false;
should_log(info, _)      -> true;
should_log(warn, debug)  -> false;
should_log(warn, info)   -> false;
should_log(warn, _)      -> true;
should_log(error, error) -> true;
should_log(error, _)     -> false;
should_log(_, _)         -> false.

log_prefix(debug) -> "DEBUG: ";
log_prefix(info)  -> "INFO:  ";
log_prefix(warn)  -> "WARN:  ";
log_prefix(error) -> "ERROR: ".

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
            ?ERROR("failed to start Erlang distributed ~p", [Error]),
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
            ?ERROR("failed to connect to remote node ~p", [Node]),
            halt(1, [{flush, true}])
    end.

plugins(Args) ->
    PluginPaths = proplists:get_all_values(plugin, Args),
    ?DEBUG("Plugins ~p~n", [PluginPaths]),
    ok = scan_plugins(PluginPaths).

scan_plugins([Plugin | Rest]) ->
    %% Load .beam files into code server
    %% or start an application if .app file exists
    EbinFiles = filelib:wildcard(Plugin ++ "/ebin/*"),
    Fun = fun(File) ->
                  load_plugin(File, filename:extension(File))
          end,
    lists:foreach(Fun, EbinFiles),

    %% Load all files from priv directory to ets
    PrivPrefix = filename:join([filename:basename(Plugin), "priv"]),
    PrivPrefix = filelib:fold_files(filename:join([Plugin, "priv"]), "", true,
                                    fun load_plugin_priv/2, PrivPrefix),

    scan_plugins(Rest);
scan_plugins([]) ->
    ok.

load_plugin(File, ".beam") ->
    Module = filename:rootname(filename:basename(File)),
    ?DEBUG("Loading plugin beam: ~s~n", [Module]),
    {module, _} = code:load_file(list_to_atom(Module));
load_plugin(File, ".app") ->
    ?DEBUG("Adding path ~s~n", [filename:dirname(File)]),
    true = code:add_path(filename:dirname(File)),
    App = filename:basename(File, ".app"),
    ?INFO("Starting plugin app: ~s~n", [App]),
    ok = application:start(list_to_atom(App)).

load_plugin_priv(File, PrivPrefix) ->
    case filelib:is_dir(File) of
        false ->
            ReOpts = [{return, list}, {parts, 2}],
            [_, FileName] = re:split(File, PrivPrefix, ReOpts),
            ?DEBUG("Loading plugin priv file: ~s~n", [FileName]),
            true = ets:insert(epl_priv, {filename:join([PrivPrefix, FileName]),
                                         file_contents(File)}),
            PrivPrefix;
        true ->
            PrivPrefix
    end.

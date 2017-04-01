%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_app).
-behaviour(application).

-include_lib("epl/include/epl.hrl").

%% Application callbacks
-export([start/2, stop/1]).

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

    %% Try to start Elixir
    maybe_start_elixir(Args),

    insert_node_name(Node),

    %% Start top supervisor
    {ok, Pid} = epl_sup:start_link(),

    %% Start EPL tracer on remote node
    {ok, _} = epl_sup:start_child(epl_tracer, [Node]),

    %% Start EPL Dashboard
    {ok, _} = epl_sup:start_child(epl_dashboard, []),

    %% Start EPL Dashboard
    {ok, _} = epl_sup:start_child(epl_traffic, []),

    %% load plugins
    PluginApps = plugins(Args),

    %% add all EPL handlers to the supervision tree
    run5(PluginApps, Args),

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
            ?ERROR("~s ~p~n", [Reason, Data]),
            help()
    end.

%% Check if node argument has been passed
run2(Args) ->
    case proplists:lookup(node, Args) of
        none ->
            ?ERROR("provide node name, e.g. --node mynode@127.0.0.1~n", []),
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
            ?ERROR("timed out when collecting settings"
                " of remote node ~p~n", [Node]),
            halt(1, [{flush, true}])
    end.

%% Load priv files to ets
run4() ->
    %% check if we run from escript or regular release
    %% if the latter, load application files from local file system
    {ok, AppFiles} = case init:get_argument(progname) of
                         {ok,[["erl"]]} ->
                             escript_foldl();
                         {ok,[["erlangpl"]]} ->
                             filesystem_foldl()
                     end,
    lists:foreach(fun load_app_files/1, AppFiles).

run5(PluginApps, Args) ->
    %% Find modules that end with _EPL
    %% by convention such modules implement EPL plugins
    LoadedModules = code:all_loaded(),
    GetHandlers = fun({_,preloaded}, Acc) ->
                          Acc;
                     ({Mod, Path}, Acc) ->
                          case re:run(Path,"EPL.beam\$",[global]) of
                              {match,_} -> [Mod | Acc];
                              nomatch   -> Acc
                          end
                  end,

    PluginModules = lists:foldl(GetHandlers, [], LoadedModules),
    Node = epl:lookup(node),

    %% Call EPL plugin callback functions to initialize plugins
    Pids = [{M, {ok, _} = epl_sup:start_child(M, [Node])}
            || M <- PluginModules],

    lists:foreach(
      fun(M) ->
              {ok, PluginConf} = M:init(Node),
              case proplists:lookup(menu_item, PluginConf) of
                  none ->
                      ?WARN("Plugin ~p:init/1 does not return menu_item attribute~n", [M]);
                  {menu_item, PluginMenu} ->
                      epl:add_plugin_menu(PluginMenu)
              end
      end,
      PluginModules),

    ?INFO("Started plugins: ~p~n", [Pids]),

    %% Configure cowboy with paths to plugin module and plugin priv files
    CowboyHandlers =
          [{"/"++atom_to_list(Mod), Mod, []} || Mod <- PluginModules],

    PrivFiles = [{"/"++App++"/[...]", epl_static, App}
                 || App <- PluginApps],

    Dispatch = cowboy_router:compile(
                 [{'_', CowboyHandlers ++ PrivFiles ++
                       [{"/[...]", epl_static, "epl"}]}
                 ]),

    Port = case proplists:lookup(port, Args) of
               none -> 8000;
               {port, N} when is_list(N) -> list_to_integer(N);
               {port, N} when is_integer(N) -> N
           end,

    {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    ?CONSOLE("Visit http://localhost:~w/~n", [Port]).

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
     {port,    $P, "port",      integer,   "HTTP and WS port number"},
     {version, $V, "version",   undefined, "Show version information"},
     {sname,   $s, "sname",     string,    "Start with a shortname"},
     {name,    $l, "name",      string,    "Start with a longname, default "
                                           "erlangpl@127.0.0.1"},
     {elixir_path, $e, "with-elixir", string, "Path to Elixir root directory"}
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

load_app_files({File, Bin}) ->
    case filename:extension(File) of
        <<".app">> ->
            ok;
        <<".beam">> ->
            FileStr = binary_to_list(File),
            ModStr = filename:basename(FileStr, ".beam"),
            Mod = list_to_atom(ModStr),
            case code:is_loaded(Mod) of
                {file, _} ->
                    ok;
                false ->
                    {module, _} = code:load_binary(Mod, FileStr, Bin)
            end;
        _ ->
            ?DEBUG("Loading: ~s~n", [File]),
            true = ets:insert(epl_priv, {File, Bin})
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
            ?ERROR("failed to start Erlang distributed ~p~n", [Error]),
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
            ?ERROR("failed to connect to remote node ~p~n", [Node]),
            halt(1, [{flush, true}])
    end.

plugins(Args) ->
    PluginPaths = proplists:get_all_values(plugin, Args),
    ?INFO("Plugins ~p~n", [PluginPaths]),
    scan_plugins(PluginPaths, []).

scan_plugins([Plugin | Rest], PluginApps) ->
    %% start an application if .app file exists
    %% TODO: What if there is no .app file? Shall we load .beam anyway?
    EbinPath = filename:join(Plugin, "ebin"),
    ?INFO("Adding path ~s~n", [EbinPath]),
    true = code:add_path(EbinPath),
    EbinFiles = filelib:wildcard(EbinPath ++ "/*"),
    Fun = fun(File, App) ->
                  case filename:extension(File) of
                      ".app" ->
                          AppName = filename:basename(File, ".app"),
                          AppPath = filename:dirname(File),
                          load_plugin(AppName, AppPath),
                          AppName;
                      ".beam" ->
                          Module = filename:rootname(filename:basename(File)),
                          ?DEBUG("Loading plugin beam: ~s~n", [Module]),
                          {module, _} = code:load_file(list_to_atom(Module)),
                          App
                  end
          end,
    case lists:foldl(Fun, undefined, EbinFiles) of
        undefined ->
            scan_plugins(Rest, PluginApps);
        AppName ->
            ok = application:start(list_to_atom(AppName)),
            scan_plugins(Rest, [AppName|PluginApps])
    end;
scan_plugins([], PluginApps) ->
    PluginApps.

load_plugin(AppName, AppPath) ->
    %% Load all files from priv directory to ets
    PluginPrivDir = filename:join([AppPath, "../priv"]),
    filelib:fold_files(PluginPrivDir, "", true,
                       fun load_plugin_priv/2, list_to_binary(AppName)),

    ?INFO("Starting plugin app: ~s~n", [AppName]),
    ok = application:load(list_to_atom(AppName)),

    AppName.

load_plugin_priv(File, App) ->
    case filelib:is_dir(File) of
        false ->
            [_, FileName] = re:split(File, "/priv/", []),
            FilePath = << App/binary, "/priv/", FileName/binary >>,
            ?DEBUG("Loading plugin priv file: ~s~n", [FilePath]),
            true = ets:insert(epl_priv, {FilePath, file_contents(File)}),
            App;
        true ->
            App
    end.

insert_node_name(Node) ->
    IndexHtml = <<"epl/priv/htdocs/index.html">>,
    NodeBin = list_to_binary(atom_to_list(Node)),
    [{_, Bin}] = ets:lookup(epl_priv, IndexHtml),
    NewBin = re:replace(Bin, <<"<!--NODE-->">>,
                        <<NodeBin/binary, $&>>,
                        [{return,binary}]),
    ets:insert(epl_priv, {IndexHtml, NewBin}).

maybe_start_elixir(Args) ->
    case proplists:lookup(elixir_path, Args) of
        none ->
            ok;
        {elixir_path, Path} ->
            code:add_patha(filename:join([Path, "lib", "elixir", "ebin"]))
    end,
    case application:ensure_all_started(elixir) of
        {ok, _} ->
            ?DEBUG("Successfully loaded and started Elixir~n", []);
        _ ->
            ?DEBUG("Couldn't start Elixir~n", [])
    end.

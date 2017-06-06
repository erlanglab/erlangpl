%%%-------------------------------------------------------------------
%% @doc epl_viz_map module.
%% Provides API to create Vizceral map.
%% @end
%%%-------------------------------------------------------------------

-module(epl_viz_map).

%% API
-export([new/1,
         push_region/2,
         pull_region/2,
         push_region/3,
         push_node/4,
         binarify/1,
         namify/1]).

%% Types
-export_type([name/0]).

-type name() :: list() | atom() | pid() | port() | binary().

%%====================================================================
%% API functions
%%====================================================================

%% @doc Creates new Vizceral map.
-spec new(EntryNode :: binary()) -> map().
new(EntryNode) ->
    entity(global, "edge", [], #{connections => [], entryNode => EntryNode}).

%% ---------------------- Regions ---------------------
%% @doc Pushes region into `Vizceral' map.
-spec push_region(Name :: name(), Vizceral :: map()) -> map().
push_region(Name, Vizceral) ->
    push_region(Name, #{}, Vizceral).

%% @doc Pushes region with `Additional' information into `Vizceral' map.
-spec push_region(Name :: name(), Additional :: map(), Vizceral :: map()) -> 
                  map().
push_region(Name, Additional, Vizceral) ->
    %% We assume that INTERNET is entryNode for every region
    %% Vizceral has backward compatibility and in region view INTERNET node is
    %% default entryNode if other isn't specified
    A = maps:merge(#{
                      connections => [],
                      maxVolume => 5000
                    },
                   Additional),
    push_node(region, Name, A, Vizceral).

%% @doc Gets region's info from `Vizceral' map.
-spec pull_region(Name :: name(), Vizceral :: map()) -> {map(), map()}.
pull_region(Name, Vizceral) ->
    {Region, Newlist} = pull_node(Name, Vizceral),
    {Region, maps:merge(Vizceral, #{nodes => Newlist})}.

%% ---------------------- Nodes -----------------------
%% @doc Pushes node into Vizceral `Enity'.
-spec push_node(Renderer :: atom(), Name :: name(), Additional :: map(),
               Entity :: map()) -> map().
push_node(Renderer, Name, Additional, Entity) ->
    #{nodes := Nodes} = Entity,
    Newnode = entity(Renderer, Name, [], Additional),
    maps:merge(Entity, #{nodes => [Newnode | Nodes]}).

%% @doc Gets node's info from Vizceral `Entity'.
-spec pull_node(Name :: name(), Entity :: map()) -> {map(), list()}.
pull_node(Name, Entity) ->
    #{nodes := Nodes} = Entity,
    {[Node], Rest} = lists:partition(
                       fun(A) ->
                               maps:get(name, A) == namify(Name)
                       end, Nodes),
    {Node, Rest}.

%% @doc Pushes additional `Info' into cluster nodes section in `Vizceral' map.
-spec push_additional_node_info(Info :: map(), Vizceral :: #{nodes => [map()]})
                               -> map().
push_additional_node_info(Info, Vizceral = #{nodes := [Node]}) ->
    UpdatedNode = maps:merge(Node, Info),
    maps:merge(Vizceral, #{nodes => [UpdatedNode]}).

%%----------------------- Names -----------------------
%% @doc Transforms `Name' to binary.
-spec binarify(Name :: name()) -> binary().
binarify(Name) when is_list(Name) ->
    list_to_binary(Name);
binarify(Name) when is_atom(Name) ->
    atom_to_binary(Name, latin1);
binarify(Name) when is_pid(Name) ->
    list_to_binary(pid_to_list(Name));
binarify(Name) when is_port(Name) ->
    list_to_binary(erlang:port_to_list(Name));
binarify(Name) when is_binary(Name) ->
    Name.

%% @doc Transforms `Name' to particular format.
-spec namify(Name :: name()) -> binary().
namify(Name) when is_binary(Name) ->
    Name1 = binary:replace(binarify(Name), <<"@">>, <<"_at_">>),
    Name2 = binary:replace(binarify(Name1), <<"<">>, <<"">>),
    Name3 = binary:replace(binarify(Name2), <<">">>, <<"">>),
    binary:replace(binarify(Name3), <<".">>, <<"_">>, [global]);
namify(Name) ->
    namify(binarify(Name)).

%%====================================================================
%% Internals
%%====================================================================

entity(Renderer, Name, Nodes, Additional) ->
    Map = #{
      renderer => Renderer,
      name => namify(Name),
      displayName => binarify(Name),
      nodes => Nodes
     },
    maps:merge(Map, Additional).

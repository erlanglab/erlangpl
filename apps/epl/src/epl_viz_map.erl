%%%-------------------------------------------------------------------
%% @doc epl_viz_map module.
%% Provides API to create Vizceral map.
%% @end
%%%-------------------------------------------------------------------

-module(epl_viz_map).

%% API
-export([new/1,
         entity/4,
         push_region/2,
         pull_region/2,
         push_region/3,
         push_node/4,
         binarify/1,
         namify/1]).

new(EntryNode) ->
    entity(global, "edge", [], #{connections => [], entryNode => EntryNode}).

entity(Renderer, Name, Nodes, Additional) ->
    Map = #{
      renderer => Renderer,
      name => namify(Name),
      displayName => binarify(Name),
      nodes => Nodes
     },
    maps:merge(Map, Additional).

%% ---------------------- Regions ---------------------
push_region(Name, Vizceral) ->
    push_region(Name, #{}, Vizceral).

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

pull_region(Name, Vizceral) ->
    {Region, Newlist} = pull_node(Name, Vizceral),
    {Region, maps:merge(Vizceral, #{nodes => Newlist})}.

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

%%----------------------- Names -----------------------
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

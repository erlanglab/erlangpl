%%%-------------------------------------------------------------------
%% @doc epl_ets_viz_data module.
%% Build data structure for vizceral graph.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets_viz_map).

%% API
-export([push_additional_node_info/2]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Pushes additional `Info` into cluster nodes section in Vizceral map.
-spec push_additional_node_info(Info :: map(), Viz :: #{nodes => [map()]}) 
                               -> map().
push_additional_node_info(Info, Viz = #{nodes := [Node]}) ->
    UpdatedNode = maps:merge(Node, Info),
    maps:merge(Viz, #{nodes => [UpdatedNode]}).

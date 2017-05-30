%%%-------------------------------------------------------------------
%% @doc epl_ets_viz_data module.
%% Build data structure for vizceral graph.
%% @end
%%%-------------------------------------------------------------------

-module(epl_ets_viz_map).

%% API
-export([push_ets_basic_info/2]).

%%====================================================================
%% API functions
%%====================================================================

push_ets_basic_info(ETSInfo, Viz = #{nodes := [Node]}) ->
    UpdatedNode = maps:merge(Node, ETSInfo),
    maps:merge(Viz, #{nodes => [UpdatedNode]}).
%%====================================================================
%% Internals 
%%====================================================================

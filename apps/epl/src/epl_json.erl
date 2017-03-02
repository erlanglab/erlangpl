%%%-------------------------------------------------------------------
%%% @doc
%%% JSON encoding and decoding
%%% @end
%%%-------------------------------------------------------------------

-module(epl_json).

-export([encode/2,
         decode/1]).

encode(Term, Topic) ->
    jsone:encode(#{<<"topic">> => Topic,
                   <<"data">> => Term}).

decode(Data) ->
    jsone:decode(Data).

%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_humio).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Wpool
-export([handle_event/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {subscribers = []}).
-define(HTTP_POOL_NAME, humio_http).
-define(HUMIO_API_URL, "https://go.humio.com").
-define(HUMIO_DATASPACE, "erlangpl").
-define(HUMIO_SOURCE_NAME, <<"erlangpl">>).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    WorkerNum = 100,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [WorkerNum], []).

%%% gen_server callbacks
%%%===================================================================
init([WorkerNum]) ->
    ok = epl:subscribe(),
    {ok, _} = wpool:start_sup_pool(?HTTP_POOL_NAME, [{workers, WorkerNum}]),
    {ok, #state{}}.

handle_call(Request, _From, _State) ->
    exit({not_implemented, Request}).

handle_cast(Request, _State) ->
    exit({not_implemented, Request}).

handle_info({data, {N, T}, Proplist} = Data, State) ->
    wpool:cast(?HTTP_POOL_NAME, {?MODULE, handle_event, [Data]}, available_worker),
    {noreply, State};
handle_info(Info, _State) ->
    exit({not_implemented, Info}).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event(Data) ->
    epl:log(debug, "Humio event ~p~n", [Data]),
    URL = humio_push_url(),
    [{humio_token, Token}] = epl:lookup(humio_token),
    Headers = [
               {"Authorization", "Bearer " ++ Token},
               {"Content-type", "application/json"},
               {"Accept", "application/json"} 
              ],
    HTTPOpts = [],
    Opts = [],
    JSON = event_to_json_map(Data),
    Body = jsone:encode(JSON),
    case httpc:request(post, {URL, Headers, "application/json", Body}, HTTPOpts, Opts) of
        {ok, {{_Proto, 200, _StatusText}, _Headers, _Payload}} ->
            epl:log(debug, "Humio HTTP: report submitted!~n", []),
            ok;
        {ok, {{_Proto, StatusCode, _StatusText}, _Headers, _Payload}} ->
            epl:log(error, "Humio HTTP: Unable to report event. Invalid HTTP status code ~p~n",
                    [StatusCode]),
            {error, {invalid_status, StatusCode}};
        {error, Reason} ->
            epl:log(error, "Humio HTTP: Unable to report event. Unknown error ~p~n",
                    [Reason]),
            {error, Reason}
    end.

humio_push_url() ->
    ?HUMIO_API_URL ++ "/api/v1/dataspaces/" ++ ?HUMIO_DATASPACE ++ "/ingest".

event_to_json_map({data, {Node, Timestamp}, Proplist}) ->
    [
     #{
        tags => #{
          host => atom_to_binary(Node, utf8),
          source => ?HUMIO_SOURCE_NAME
         },
        events => [#{
                      timestamp => format_timestamp(Timestamp),
                      attributes => format_attributes(Proplist)
                    }]
      }
    ].

%% Fixme!: timezone?
format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Timestamp),
    list_to_binary(lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w+00:00",[Year,Month,Day,Hour,Minute,Second]))).

format_attributes(Proplist) ->
    lists:foldl(
      fun({spawn, Spawn}, MapIn) ->
              maps:put(spawn, epl:to_bin(length(Spawn)), MapIn);
         ({exit, Exit}, MapIn) ->
              _Abnormal = lists:foldl(
                            fun({_,normal,_}, Acc) -> Acc;
                               (_, Acc) -> Acc + 1
                            end,
                            0, Exit),
              maps:put(exit, epl:to_bin(length(Exit)), MapIn);
         ({'receive', Receive}, MapIn) ->
              {Count, _Sizes} = lists:foldl(
                                  fun({_,C,S}, {Count, Sizes}) ->
                                          {Count+C, Sizes+S}
                                  end,
                                  {0, 0}, Receive),
              maps:put('receive', epl:to_bin(Count), MapIn);
         ({process_count, Value}, MapIn) ->
              maps:put(process_count, Value, MapIn);
         ({memory_total, Value}, MapIn) ->
              maps:put(memory_total, Value, MapIn);
         (_Unknown, MapIn) ->
              MapIn
      end, #{}, Proplist).

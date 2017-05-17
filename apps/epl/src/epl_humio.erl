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
    epl:log(debug, "Humio event ~p: ~p", [{N, T}, Proplist]),
    wpool:cast(?HTTP_POOL_NAME, {?MODULE, handle_event, [Data]}, available_worker),
    {noreply, State};
handle_info(Info, _State) ->
    exit({not_implemented, Info}).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event(Data) ->
    epl:log(debug, "Humio event ~p", [Data]),
    URL = ?HUMIO_API_URL ++ "/api/v1/dataspaces/" ++ ?HUMIO_DATASPACE ++ "/ingest",
    Token = epl:lookup(humio_token),
    Headers = [
        {"Authorization", "Bearer " ++ Token},
        {"Content-type", "application/json"},
        {"Accept", "application/json"} 
    ],
    HTTPOpts = [],
    Opts = [],

    JSON = event_to_json_map(Data),

    Body = jsone:encode(JSON),
    Res = httpc:request(post, {URL, Headers, "application/json", Body}, HTTPOpts, Opts),
    epl:log(debug, "Humio HTTP res ~p", [Res]),
    ok.

event_to_json_map({data, {Node, Timestamp}, Proplist}) ->
    [
     #{
        tags => #{
          host => atom_to_binary(Node, utf8),
          source => <<"ErlangPL">>
        },
        events => [#{
          timestamp => format_timestamp(Timestamp),
          attributes => format_attributes(Proplist)
        }]
     }
    ].

format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Timestamp),
    list_to_binary(lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w+00:00",[Year,Month,Day,Hour,Minute,Second]))).

format_attributes(Proplist) ->
    maps:map(
      fun(spawn, Spawn) ->
              epl:to_bin(length(Spawn));
         (exit, Exit) ->
              Abnormal = lists:foldl(
                           fun({_,normal,_}, Acc) -> Acc;
                              (_, Acc) -> Acc + 1
                           end,
                           0, Exit),
              [{<<"count">>, epl:to_bin(length(Exit))},
               {<<"abnormal">>, epl:to_bin(Abnormal)}];
         ('receive', Receive) ->
              {Count, Sizes} = lists:foldl(
                                 fun({_,C,S}, {Count, Sizes}) ->
                                         {Count+C, Sizes+S}
                                 end,
                                 {0, 0}, Receive),
              epl:to_bin(Count);
         (process_count, Value) ->
              Value;
         (memory_total, Value) ->
              Value;
         (Item, Value) ->
              invalid
      end,
      maps:from_list(Proplist)).

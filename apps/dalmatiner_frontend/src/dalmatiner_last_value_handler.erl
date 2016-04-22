%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(dalmatiner_last_value_handler).

-include_lib("mmath/include/mmath.hrl").

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

-define(LOOKUP_PERIOD, 3600).

%% Public Api
%% ==========

init(_Transport, Req, []) ->
    {ok, Req, undefined}.


-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    Req0 = cowboy_req:set_resp_header(
             <<"access-control-allow-origin">>, <<"*">>, Req),
    {Bucket, Req1} = cowboy_req:binding(bucket, Req0),
    {Path, Req2} = cowboy_req:path_info(Req1),
    {ContentType, Req3} = dalmatiner_idx_handler:content_type(Req2),
    D = get_last_value(Bucket, Path),
    dalmatiner_idx_handler:send(ContentType, D, Req3, State).

terminate(_Reason, _Req, State) ->
    {ok, State}.

%% Private functions
%% =================

%% Get last value.
%%
%% To reduce impact on IO operation we do 2 phase fetch.
%% First lookup is for just INITIAL_LOOKUP_PERIOD seconds up to INITIAL_LOOKUP_DELAY.
%% With current parameters it has roughly 30% chance of being answered completely from
%% memory. If no point is found during that lookup, we fire another full lookup to search
%% point in further EXTRA_LOOKUP_PERIOD.
%%
get_last_value(Bucket, Path) ->
    Metric = dproto:metric_from_list(Path),
    Now = erlang:system_time(seconds),
    get_last_value(Bucket, Metric, Now - ?LOOKUP_PERIOD, ?LOOKUP_PERIOD).

get_last_value(Bucket, Metric, Time, Period) ->
    Count = Period, %% TODO: we should take into account bucket resolution and maybe iterate in chunks
    {ok, _Resolution, Data} = ddb_connection:get(Bucket, Metric, Time, Count),
    {Value, Offset} = find_last_point(Data),
    case Value of
        none -> null;
        _ -> [{t, Time + Offset}, {v, Value}]
    end.

find_last_point(Data) ->
    find_last_point(Data, {size(Data), - ?DATA_SIZE}).

%% TODO: probably instead of using low level macro we shoud add some
%% mmath utility to make it more abstract.
find_last_point(<<>>, {Pos, _Len}) ->
    {none, Pos / ?DATA_SIZE};
find_last_point(_Data, {0, _Len}) ->
    {none, 0};
find_last_point(Data, Part) ->
    PData = binary:part(Data, Part),
    <<T:?TYPE_SIZE, _/binary>> = PData,
    {Pos, Len} = Part,
    case T of
        ?NONE -> find_last_point(Data, {Pos + Len, Len});
        _ ->
            [Value] = mmath_bin:to_list(PData),
            {Value, (Pos div ?DATA_SIZE) - 1}
    end.

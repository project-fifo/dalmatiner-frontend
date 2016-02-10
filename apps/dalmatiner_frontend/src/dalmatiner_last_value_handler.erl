%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(dalmatiner_last_value_handler).

-include_lib("mmath/include/mmath.hrl").

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

%% How long to look up for last value (2 Hours)
-define(LOOKUP_PERIOD, 2 * 3600).

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

get_last_value(Bucket, Path) ->
    Metric = dproto:metric_from_list(Path),
    Now = erlang:system_time(seconds),
    Time = Now - ?LOOKUP_PERIOD,
    Count = ?LOOKUP_PERIOD, %% TODO: we should take into account bucket resolution and maybe iterate in chunks
    {ok, _Resolution, Data} = dalmatiner_connection:get(Bucket, Metric, Time, Count),
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

%% @doc POST echo handler.
-module(dalmatiner_inspect_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    Req0 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                                      <<"*">>, Req),
    case cowboy_req:qs_val(<<"q">>, Req0) of
        {undefined, Req1} ->
            {ok, Req2} = cowboy_req:reply(
                           400,
                           [{<<"content-type">>, <<"text/plain">>}],
                           "Missing required q= parameter",
                           Req1),
            {ok, Req2, State};
        {Q, Req1} ->
            case dqe:prepare(Q) of
                {error, E} ->
                    Error = list_to_binary(dqe:error_string({error, E})),
                    {ok, Req2} =
                        cowboy_req:reply(400, [], Error, Req1),
                    {ok, Req2, State};
                {ok, {Parts, Start, Count}} ->
                    D = [{<<"b">>, inspect_buckets(Parts)},
                         {<<"s">>, Start},
                         {<<"c">>, Count}],
                    {ContentType, Req2} = dalmatiner_idx_handler:content_type(Req1),
                    dalmatiner_idx_handler:send(ContentType, D, Req2, State)
            end
    end.

terminate(_Reason, _Req, State) ->
    {ok, State}.

inspect_buckets(Parts) ->
    BSet = get_buckets(Parts, sets:new()),
    sets:to_list(BSet).

get_buckets([], Acc) ->
    Acc;
get_buckets([Part | Rest], Acc) ->
    Acc1 = get_part_buckets(Part, Acc),
    get_buckets(Rest, Acc1).

get_part_buckets({dqe_get, [Bucket, _Metric]}, Acc) ->
    sets:add_element(Bucket, Acc);
get_part_buckets({_Operand, []}, Acc) ->
    Acc;
get_part_buckets({_Operand, [Nested | Rest]}, Acc) ->
    Acc1 = case is_tuple(Nested) of
               true -> get_part_buckets(Nested, Acc);
               false -> Acc
           end,
    get_part_buckets({null, Rest}, Acc1);
get_part_buckets({_Operand, Nested}, Acc) when is_tuple(Nested) ->
    get_part_buckets(Nested, Acc).

-module(dalmatiner_dl_auth_hook).
%% TODO: Probably we should have authentication written as middleware: http://ninenines.eu/docs/en/cowboy/1.0/guide/middlewares/

-export([handle/1]).


-spec handle(cowboy_req:req()) -> {ok | noauth, cowboy_req:req()}.
handle(Req) ->
    {Auth, Req1} = cowboy_req:header(<<"authorization">>, Req),
    [AuthType, JWT] = binary:split(Auth, <<" ">>),
    "bearer" = string:to_lower(binary_to_list(AuthType)),
    {ok, Key} = application:get_env(dalmatiner_frontend, jwt_secret),
    Payload = ejwt:decode(JWT, Key),
    Req2 = populate_req_meta(Payload, Req1),
    case cowboy_req:meta(dl_auth_is_authenticated, Req2) of
        {true, Req3} ->
            {ok, Req3};
        {_, Req3} ->
            {noauth, Req3}
    end.


populate_req_meta([], Req) ->
    Req;
populate_req_meta([{<<"scopes">>, S} | Rest], Req) ->
    Req1 = cowboy_req:set_meta(dl_auth_scopes, S, Req),
    populate_req_meta(Rest, Req1);
populate_req_meta([{<<"aud">>, U} | Rest], Req) ->
    Req1 = cowboy_req:set_meta(dl_auth_user, U, Req),
    populate_req_meta(Rest, Req1);
populate_req_meta([{<<"exp">>, E} | Rest], Req) ->
    IsAuth = erlang:system_time(seconds) =< E,
    Req1 = cowboy_req:set_meta(dl_auth_is_authenticated, IsAuth, Req),
    populate_req_meta(Rest, Req1);
populate_req_meta([_ | Rest], Req) ->
    populate_req_meta(Rest, Req).

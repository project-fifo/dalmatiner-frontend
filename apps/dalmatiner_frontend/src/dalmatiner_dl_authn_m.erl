-module(dalmatiner_dl_authn_m).
-behaviour(cowboy_middleware).

-export([execute/2]).


-spec execute(cowboy_req:req(), [{atom(), any()}]) ->
                     {ok, cowboy_req:req(), [{atom(), any()}]} |
                     {halt, cowboy_req:req()}.
execute(Req, Env) ->
    try authenticate(Req) of
        {ok, Req1} ->
            {ok, Req1, Env};
        {halt, Req1} ->
            {halt, Req1}
    catch
        Exception:Reason ->
            Stack = erlang:get_stacktrace(),
            lager:error("Error in authorization hook (~p:~p): ~p", [Exception, Reason, Stack]),
            {error, 500, Req}
    end.


%%
%% Private functions
%% =================

authenticate(Req) ->
    %% Cowboy requires middleware to wrap it's own exceptions. Otherwise
    %% you get empty response with very little information about what happened.
    {Auth, Req1} = cowboy_req:header(<<"authorization">>, Req),
    %% TODO: Respond with 401 when no authorization token and try to lookup token in url params
    [AuthType, JWT] = binary:split(Auth, <<" ">>),
    "bearer" = string:to_lower(binary_to_list(AuthType)),
    {ok, Key} = application:get_env(dalmatiner_frontend, jwt_secret),
    Payload = ejwt:decode(JWT, Key),
    Req2 = populate_req_meta(Payload, Req1),
    case cowboy_req:meta(dl_auth_is_authenticated, Req2) of
        {true, Req3} ->
            {ok, Req3};
        {_, Req3} ->
            {ok, Req4} = cowboy_req:reply(403, Req3),
            {halt, Req4}
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

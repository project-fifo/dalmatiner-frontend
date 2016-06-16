-module(dalmatiner_dl_authz_m).
-behaviour(cowboy_middleware).

-export([execute/2]).

%%
%% Authentycation middleware entry point
%% =====================================

-spec execute(cowboy_req:req(), [{atom(), any()}]) ->
                     {ok, cowboy_req:req(), [{atom(), any()}]} |
                     {halt, cowboy_req:req()}.
execute(Req, Env) ->
    try authorize(Req, Env) of
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

authorize(Req, Env) ->
    Acl = proplists:get_value(acl, Env),
    case eval_acl(Acl, Req) of
        {allow, Req1} ->
            {ok, Req1};
        {deny, Req1} ->
            {ok, Req2} = cowboy_req:reply(403, Req1),
            {halt, Req2}
    end.

eval_acl([], Req) ->
    {allow, Req};
eval_acl([{Pattern, Perm} | Rest], Req) ->
    case match(Pattern, Req) of
        {true, Req1} ->
            assert(Perm, Req1);
        {_, Req1} ->
            eval_acl(Rest, Req1)
    end.

match({path, Pattern}, Req) ->
    {Path, Req1} = cowboy_req:path(Req),
    case match_binary(Path, Pattern) of
        true ->
            {true, Req1};
        _ ->
            {false, Req1}
    end.

match_binary(Subject, Pattern) when size(Pattern) =< size(Subject) ->
    PSize = size(Pattern),
    case <<Subject:PSize/binary>> of
        Pattern -> true;
        _ -> false
    end;
match_binary(_, _) ->
    false.

assert(require_authenticated, Req) ->
    case cowboy_req:meta(dl_auth_is_authenticated, Req) of
        {true, Req1} ->
            {allow, Req1};
        {_, Req1} ->
            {deny, Req1}
    end;
assert(require_collection_access, Req) ->
    {OrgId, Req1} = cowboy_req:binding(collection, Req),
    {UserId, Req2} = cowboy_req:meta(dl_auth_user, Req1),
    case cowboy_req:meta(dl_auth_is_authenticated, Req) of
        {true, Req2} ->
            {ok, Access} = dalmatiner_dl_data:user_org_access(UserId, OrgId),
            {Access, Req2};
        {_, Req1} ->
            {deny, Req2}
    end.

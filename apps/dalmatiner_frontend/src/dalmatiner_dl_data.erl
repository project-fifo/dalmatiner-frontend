-module(dalmatiner_dl_data).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).
-export([user_orgs/1, user_org_access/2, agent_access/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(POOL, dalmatiner_dl_data_pool).
-define(TIMEOUT, 30000).
-record(state, {connection}).

%% TODO: Add debug timing arround DB queries

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec user_orgs(binary()) -> {ok, [#{}]}.
user_orgs(UserId) ->
    call({user_orgs, UserId}).

-spec user_org_access(binary(), binary()) -> {ok, allow | deny}.
user_org_access(UserId, OrgId) ->
    call({user_org_access, UserId, OrgId}).

-spec agent_access(binary(), [{<<_:96>>}]) -> {ok, allow | deny}.
agent_access(Finger, OrgOids) ->
    call({agent_access, Finger, OrgOids}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

call(Request) ->
    poolboy:transaction(?POOL,
                        fun(S) ->
                                gen_server:call(S, Request, ?TIMEOUT)
                        end, ?TIMEOUT).

init(ConnectionArgs) ->
    {ok, C} = mc_worker_api:connect(ConnectionArgs),
    {ok, #state{connection = C}, 0}.

handle_call({user_orgs, UserId}, _From, #state{connection = C} = State) ->
    Orgs = find_user_orgs(C, UserId),
    {reply, {ok, Orgs}, State};
handle_call({user_org_access, UserId, OrgId}, _From, #state{connection = C} = State) ->
    Access = check_user_org_access(C, UserId, OrgId),
    {reply, {ok, Access}, State};
handle_call({agent_access, Finger, OrgOids}, _From, #state{connection = C} = State) ->
    Access = check_agent_access(C, Finger, OrgOids),
    {reply, {ok, Access}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State ) ->
    {noreply, State}.

terminate(_Reason, #state{connection = C}) ->
    _ = mc_worker_api:disconnect(C),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_user_orgs(C, UserId) ->
    Gids = find_user_group_ids(C, UserId),
    Orgs = find_orgs_for_gids(C, Gids),
    %% TODO: We need to expand tenants as well
    Orgs.

find_user_group_ids(C, UserId) ->
    UserOid = {base16:decode(UserId)},
    Cursor = mc_worker_api:find(C, <<"groups">>,
                                  {<<"users.user">>, UserOid},
                                 #{projector => {<<"_id">>, true}}),
    Gids = mc_cursor:foldl(fun (#{<<"_id">> := Gid}, Acc) ->
                                   [Gid | Acc]
                           end, [], Cursor, infinity),
    mc_cursor:close(Cursor),
    Gids.

find_orgs_for_gids(C, Gids) ->
    Cursor = mc_worker_api:find(C, <<"orgs">>,
                                  {<<"group">>, {<<"$in">>, Gids}},
                                 #{projector => 
                                       {<<"_id">>, true,
                                        <<"name">>, true,
                                        <<"tenant">>, true}}),
    Orgs = mc_cursor:foldl(fun (O, Acc) ->
                                   [O | Acc]
                           end, [], Cursor, infinity),
    mc_cursor:close(Cursor),
    Orgs.

check_user_org_access(C, UserId, OrgId) ->
    UserOid = {base16:decode(UserId)},
    OrgOid = {base16:decode(OrgId)},
    Org = mc_worker_api:find_one(C, <<"orgs">>, {<<"_id">>, OrgOid},
                                 #{projector => {<<"group">>, true}}),
    Gid = maps:get(<<"group">>, Org, undefined),
    Group = mc_worker_api:find_one(C, <<"groups">>, 
                                   {<<"_id">>, Gid,
                                    <<"users.user">>, UserOid},
                                   #{projector => {<<"users">>, true}}),
    case Group of
        #{<<"_id">> := _} ->
            allow;
        _ ->
            deny    
    end.

check_agent_access(C, Finger, OrgOids) ->
    Agent = mc_worker_api:find_one(C, <<"agents">>,
                                   {<<"_id">>, Finger,
                                    <<"org">>, {<<"$in">>, OrgOids}},
                                   #{projector => {<<"_id">>, true}}),
    case Agent of
        #{<<"_id">> := _} ->
            allow;
        _ ->
            deny
    end.

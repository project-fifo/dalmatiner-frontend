-module(dalmatiner_dl_data).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).
-export([user_orgs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(POOL, dalmatiner_dl_data_pool).
-define(TIMEOUT, 30000).
-record(state, {connection}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


user_orgs(UserId) ->
    poolboy:transaction(?POOL,
                        fun(S) ->
                                gen_server:call(S, {user_orgs, UserId}, ?TIMEOUT)
                        end, ?TIMEOUT).
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(ConnectionArgs) ->
    {ok, C} = mc_worker_api:connect(ConnectionArgs),
    {ok, #state{connection = C}, 0}.

handle_call({user_orgs, UserId}, _From, #state{connection = C} = State) ->
    Orgs = find_user_orgs(C, UserId),
    {reply, {ok, Orgs}, State};
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

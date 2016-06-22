-module(dalmatiner_frontend_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    dqe_idx:init(),
    {ok, {{one_for_one, 5, 10}, [mongo_spec()]}}.

%%====================================================================
%% Internal functions
%%====================================================================

mongo_spec () ->
    Name = dalmatiner_dl_data_pool,
    {ok, PoolS} = application:get_env(dalmatiner_frontend, mongodb_pool_size),
    {ok, PoolMax} = application:get_env(dalmatiner_frontend, mongodb_pool_max),
    PoolArgs = [{name, {local, Name}},
                {worker_module, dalmatiner_dl_data},
                {size, PoolS},
                {max_overflow, PoolMax}],
    MongoArgs = mongo_args(),
    poolboy:child_spec(Name, PoolArgs, MongoArgs).

mongo_args () ->
    A1 = case application:get_env(dalmatiner_frontend, mongodb_server) of
             {ok, {Host, Port}} ->
                 [{host, Host}, {port, Port}];
             _ ->
                 []
         end,
    A2 = get_binary_arg(database, A1),
    A3 = get_binary_arg(login, A2),
    A4 = get_binary_arg(password, A3),
    case application:get_env(dalmatiner_frontend, slave_ok) of
        {ok, true} ->
            [{r_mode, slave_ok} | A4];
        _ ->
            A4
    end.

get_binary_arg (Name, Proplist) ->
    EnvName = list_to_atom("mongo_" ++ atom_to_list(Name)),
    case application:get_env(dalmatiner_frontend, EnvName) of
        {ok, Value} ->
            BValue = list_to_binary(Value),
            [{Name, BValue} | Proplist];
        _ ->
            Proplist
    end.

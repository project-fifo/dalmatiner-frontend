-module(dalmatiner_frontend_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Listeners} = application:get_env(dalmatiner_frontend, http_listeners),
    {ok, Port} =  application:get_env(dalmatiner_frontend, http_port),

    Dispatch = cowboy_router:compile(
                 [
                  %% {URIHost, list({URIPath, Handler, Opts})}
                  {'_', [{"/", dalmatiner_idx_handler, []},
                         %% Old style API
                         {"/buckets/", dalmatiner_bucket_h, []},
                         {"/buckets/[...]", dalmatiner_key_h, []},

                         %% New style API
                         %% List all collections
                         {"/collections", dalmatiner_collection_h, []},
                         %% List all tag namespaces in collection
                         {"/collections/:collection/namespaces",
                          dalmatiner_namespace_h, []},
                         %% List all tag in a namespace
                         {"/collections/:collection/namespaces/"
                          ":namespace/tags",
                          dalmatiner_tag_h, []},
                         %% List all values in a teg
                         {"/collections/:collection/namespaces/"
                          ":namespace/tags/:tag/values",
                          dalmatiner_value_h, []},
                         %% List all metrics in a collection
                         {"/collections/:collection/metrics/",
                          dalmatiner_metric_h, []},
                         %% List all namespaces per metric
                         {"/collections/:collection/metrics/"
                          ":metric/namespaces/",
                          dalmatiner_namespace_h, []},
                         %% List all tags in a namespace per metric
                         {"/collections/:collection/metrics/"
                          ":metric/namespaces/:namespace/tags/",
                          dalmatiner_tag_h, []},
                         %% List all values in a tag per metric
                         {"/collections/:collection/metrics/"
                          ":metric/namespaces/:namespace/tags/"
                          ":tag/values",
                          dalmatiner_value_h, []},

                         %% Dataloop API extension
                         {"/status", dalmatiner_status_handler, []},
                         {"/inspect", dalmatiner_inspect_handler, []},

                         %% STatic content.
                         {"/js/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/js",
                           [{mimetypes, cow_mimetypes, web}]}},
                         {"/fonts/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/fonts",
                           [{mimetypes, cow_mimetypes, web}]}},
                         {"/css/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/css",
                           [{mimetypes, cow_mimetypes, web}]}},
                         {"/img/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/img",
                           [{mimetypes, cow_mimetypes, web}]}},
                         {"/static/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/",
                           [{mimetypes, cow_mimetypes, web}]}}]}
                 ]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    {ok, _} = cowboy:start_http(dalmatiner_http_listener, Listeners,
                                [{port, Port}],
                                [{env, [{dispatch, Dispatch}]},
                                 {onrequest, fun onrequest_hook/1},
                                 {max_keepalive, 5},
                                 {timeout, 50000}]),
    dalmatiner_frontend_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

onrequest_hook(Req) ->
    %% Cowboy will just return with empty response in case of errors inside
    try dalmatiner_dl_auth_hook:handle(Req) of
        {ok, Req1} ->
            Req1;
        {noauth, Req1} ->
            {ok, Req2} = cowboy_req:reply(403, Req1),
            Req2
    catch
        Exception:Reason ->
            Stack = erlang:get_stacktrace(),
            lager:error("Error in authorization hook (~p:~p): ~p", [Exception, Reason, Stack]),
            cowboy_req:reply(500, Req)
    end.

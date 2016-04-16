-module(t3_web_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/connect", t3_connect_handler, []},
               {"/", cowboy_static, {priv_file, t3_web, "static/index.html"}},
               {"/[...]", cowboy_static, {priv_dir, t3_web, "static/"}}
        ]}
    ]),
    {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8888}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    t3_web_sup:start_link().

stop(_State) ->
    ok.

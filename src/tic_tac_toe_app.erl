-module(tic_tac_toe_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/", cowboy_static, {priv_file, tic_tac_toe, "static/index.html"}},
               {"/connect", t3_connect_handler, []}
        ]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    tic_tac_toe_sup:start_link().

stop(_State) ->
    ok.

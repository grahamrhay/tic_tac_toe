-module(tic_tac_toe_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    tic_tac_toe_sup:start_link().

stop(_State) ->
    ok.

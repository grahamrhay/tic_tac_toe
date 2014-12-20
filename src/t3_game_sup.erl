-module(t3_game_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{t3_game, {t3_game, start_link, []}, transient, 5000, worker, [t3_game]}],
    {ok, {{simple_one_for_one, 5, 10}, Procs}}.

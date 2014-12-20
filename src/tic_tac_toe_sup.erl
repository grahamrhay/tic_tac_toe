-module(tic_tac_toe_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        {t3_session_manager, {t3_session_manager, start_link, []}, permanent, 5000, worker, [t3_session_manager]},
        {t3_match_maker, {t3_match_maker, start_link, []}, permanent, 5000, worker, [t3_match_maker]}
    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.

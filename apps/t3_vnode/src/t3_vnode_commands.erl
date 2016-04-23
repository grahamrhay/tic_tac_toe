-module(t3_vnode_commands).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start/2, play/3]).

%% @doc Start a game, with the specified id
start(GameId, Players) ->
    send(GameId, {start, GameId, Players}).

%% @doc Make a move in a game
play(GameId, Player, Cell) ->
    send(GameId, {play, GameId, Player, Cell}).

send(GameId, Msg) ->
    DocIdx = riak_core_util:chash_key({<<"tic_tac_toe">>, GameId}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, t3_vnode),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Msg, t3_vnode_master).

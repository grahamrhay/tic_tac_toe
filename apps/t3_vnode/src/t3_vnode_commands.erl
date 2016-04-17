-module(t3_vnode_commands).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0]).

-ignore_xref([ping/0]).

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, t3_vnode),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, t3_vnode_master).

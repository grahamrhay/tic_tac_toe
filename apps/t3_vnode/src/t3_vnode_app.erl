-module(t3_vnode_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case t3_vnode_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, t3_vnode}]),
            ok = riak_core_node_watcher:service_up(t3_vnode, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

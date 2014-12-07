-module(t3_connect_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
     {cowboy_websocket, Req, Opts}.

websocket_handle(Frame = {text, _}, Req, State) ->
    {reply, Frame, Req, State};

websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

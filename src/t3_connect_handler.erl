-module(t3_connect_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
     {cowboy_websocket, Req, Opts}.

websocket_handle({text, <<"new_session">>}, Req, State) ->
    Resp = start_new_session(),
    {reply, {text, Resp}, Req, State};

websocket_handle({text, <<"new_game">>}, Req, State) ->
    Resp = start_new_game(),
    {reply, {text, Resp}, Req, State};

websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

start_new_session() ->
    SessionId = uuid:get_v4(),
    jiffy:encode(#{type => <<"new_session">>, id => uuid:uuid_to_string(SessionId, binary_standard)}).

start_new_game() ->
    GameId = 1,
    jiffy:encode(#{type => <<"new_game">>, id => GameId}).

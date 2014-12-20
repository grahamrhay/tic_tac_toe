-module(t3_connect_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
     {cowboy_websocket, Req, Opts}.

websocket_handle({text, <<"new_session">>}, Req, State) ->
    Resp = start_new_session(),
    {reply, {text, Resp}, Req, State};

websocket_handle({text, <<"{\"type\":\"new_game\",\"sessionId\":\"", SessionId:36/binary, "\"}">>}, Req, State) ->
    Resp = start_new_game(SessionId),
    {reply, {text, Resp}, Req, State};

websocket_handle(Frame, Req, State) ->
    io:format("Unexpected frame: ~p~n", [Frame]),
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

start_new_session() ->
    {ok, SessionId} = gen_server:call(t3_session_manager, new_session),
    jiffy:encode(#{type => <<"new_session">>, id => uuid:uuid_to_string(SessionId, binary_standard)}).

start_new_game(SessionId) ->
    {ok, GameId} = gen_server:call(t3_match_maker, {find_game, SessionId}, 30000),
    jiffy:encode(#{type => <<"new_game">>, id => GameId}).

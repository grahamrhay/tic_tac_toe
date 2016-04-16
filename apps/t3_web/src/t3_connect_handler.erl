-module(t3_connect_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #{}}.

websocket_handle({text, <<"new_session">>}, Req, State) ->
    Resp = start_new_session(),
    {reply, make_frame(Resp), Req, State};

websocket_handle({text, Json}, Req, State) ->
    lager:info("Received frame: ~p~n", [Json]),
    Msg = jsx:decode(Json, [return_maps]),
    Resp = validate_session(Msg, fun() ->
        Type = maps:get(<<"type">>, Msg),
        handle_message(Type, Msg)
    end),
    {reply, make_frame(Resp), Req, State};

websocket_handle(Frame, Req, State) ->
    lager:error("Unexpected frame: ~p~n", [Frame]),
    {ok, Req, State}.

websocket_info({Type, Data}, Req, State) ->
    Msg = #{type => Type, data => Data},
    {reply, make_frame(Msg), Req, State};

websocket_info(Info, Req, State) ->
    lager:error("Unexpected msg: ~p~n", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

start_new_session() ->
    {ok, SessionId} = gen_server:call(t3_session_manager, new_session),
    #{type => <<"new_session">>, id => SessionId}.

validate_session(Msg, Fun) ->
    SessionId = maps:get(<<"sessionId">>, Msg),
    case gen_server:call(t3_session_manager, {validate_session, SessionId}) of
        ok -> Fun();
        invalid_session -> #{type => <<"error">>, msg=> <<"invalid_session">>}
    end.

start_new_game(_SessionId) ->
    Res = try
         gen_server:call(t3_match_maker, {find_game}, 30000)
    catch
        exit:{timeout,_} -> timeout
    end,
    case Res of
        {ok, GameId} -> #{type => <<"new_game">>, id => GameId};
        timeout -> #{type => <<"no_game_available">>}
    end.

make_frame(Msg) ->
    Json = jsx:encode(Msg),
    {text, Json}.

handle_message(<<"new_game">>, Msg) ->
    SessionId = maps:get(<<"sessionId">>, Msg),
    start_new_game(SessionId);

handle_message(<<"play">>, Msg) ->
    GameId = maps:get(<<"gameId">>, Msg),
    Cell = maps:get(<<"cell">>, Msg),
    play(GameId, Cell).

play(GameId, Cell) ->
    GamePid = gproc:lookup_pid({n, l, GameId}),
    ok = gen_fsm:send_event(GamePid, {play, self(), Cell}),
    #{}.

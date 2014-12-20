-module(t3_match_maker).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {waiting=[]}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call({find_game, SessionId}, From, State) ->
    case find_game(SessionId, From, State) of
        {ok, GameId, NewState} -> {reply, {ok, GameId}, NewState};
        {wait, NewState} -> {noreply, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

find_game(SessionId, From, #state{waiting=[]}) ->
    {wait, #state{waiting=[{SessionId, From}]}};

find_game(_P2Sid, _P2From, #state{waiting=[{_P1Sid, P1From}|Rest]}) ->
    GameId = 1,
    gen_server:reply(P1From, {ok, GameId}),
    {ok, GameId, #state{waiting=Rest}}.

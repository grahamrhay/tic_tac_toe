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

handle_call({find_game}, From, State) ->
    case find_game(From, State) of
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

find_game(From, #state{waiting=[]}) ->
    {wait, #state{waiting=[From]}};

find_game({P2,_}, #state{waiting=[From|Rest]}) ->
    GameId = 1,
    {P1,_} = From,
    {ok, _Pid} = supervisor:start_child(t3_game_sup, [{P1, P2}]),
    gen_server:reply(From, {ok, GameId}),
    {ok, GameId, #state{waiting=Rest}}.

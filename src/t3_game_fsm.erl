-module(t3_game_fsm).
-behaviour(gen_fsm).

%% API.
-export([start_link/1]).

%% gen_fsm.
-export([init/1]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).
-export([p1_turn/3]).
-export([p2_turn/3]).

-record(state, {p1, p2, board=#{
    <<"1,1">> => '_', <<"1,2">> => '_', <<"1,3">> => '_',
    <<"2,1">> => '_', <<"2,2">> => '_', <<"2,3">> => '_',
    <<"3,1">> => '_', <<"3,2">> => '_', <<"3,3">> => '_'
}}).

%% API.

start_link(Args) ->
    gen_fsm:start_link(?MODULE, [Args], []).

%% gen_fsm.

init(Args) ->
    io:format("New game started: ~p~n", [Args]),
    [{P1, P2, GameId}] = Args,
    true = gproc:reg({n, l, GameId}),
    State = #state{p1 = P1, p2 = P2},
    P1 ! {your_turn, State#state.board},
    P2 ! {wait, State#state.board},
    {ok, p1_turn, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

p1_turn({play, P1, Cell}, _From, State = #state{p1 = P1}) ->
    NewState = play(Cell, State, 'O'),
    notify_players(NewState#state.p2, NewState#state.p1, NewState#state.board),
    {reply, ok, p2_turn, NewState}.

p2_turn({play, P2, Cell}, _From, State = #state{p2 = P2}) ->
    NewState = play(Cell, State, 'X'),
    notify_players(NewState#state.p1, NewState#state.p2, NewState#state.board),
    {reply, ok, p1_turn, NewState}.

play(Cell, State, Symbol) ->
    '_' = maps:get(Cell, State#state.board),
    State#state{board = maps:update(Cell, Symbol, State#state.board)}.

notify_players(Play, Wait, Board) ->
    Play ! {your_turn, Board},
    Wait ! {wait, Board}.

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
-export([p1_turn/2]).
-export([p2_turn/2]).

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

p1_turn({play, P1, Cell}, State = #state{p1 = P1}) ->
    NewState = play(Cell, State, 'O'),
    case t3_game:has_won(NewState#state.board, 'O') of
        true ->
            game_won(NewState#state.p1, NewState#state.p2, NewState#state.board),
            {stop, normal, NewState};
        false ->
            case t3_game:is_draw(NewState#state.board) of
                true ->
                    game_drawn(NewState#state.p1, NewState#state.p2, NewState#state.board),
                    {stop, normal, NewState};
                false ->
                    notify_players(NewState#state.p2, NewState#state.p1, NewState#state.board),
                    {next_state, p2_turn, NewState}
            end
    end.

p2_turn({play, P2, Cell}, State = #state{p2 = P2}) ->
    NewState = play(Cell, State, 'X'),
    case t3_game:has_won(NewState#state.board, 'X') of
        true ->
            game_won(NewState#state.p2, NewState#state.p1, NewState#state.board),
            {stop, normal, NewState};
        false ->
            case t3_game:is_draw(NewState#state.board) of
                true ->
                    game_drawn(NewState#state.p1, NewState#state.p2, NewState#state.board),
                    {stop, normal, NewState};
                false ->
                    notify_players(NewState#state.p1, NewState#state.p2, NewState#state.board),
                    {next_state, p1_turn, NewState}
            end
    end.

play(Cell, State, Symbol) ->
    '_' = maps:get(Cell, State#state.board),
    State#state{board = maps:update(Cell, Symbol, State#state.board)}.

notify_players(Play, Wait, Board) ->
    Play ! {your_turn, Board},
    Wait ! {wait, Board}.

game_won(Win, Lose, Board) ->
    Win ! {you_win, Board},
    Lose ! {you_lose, Board}.

game_drawn(P1, P2, Board) ->
    P1 ! {draw, Board},
    P2 ! {draw, Board}.

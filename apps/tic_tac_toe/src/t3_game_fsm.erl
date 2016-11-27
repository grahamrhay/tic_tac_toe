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

-record(state, {game_id, state, p1, p2, board=#{
    <<"1,1">> => '_', <<"1,2">> => '_', <<"1,3">> => '_',
    <<"2,1">> => '_', <<"2,2">> => '_', <<"2,3">> => '_',
    <<"3,1">> => '_', <<"3,2">> => '_', <<"3,3">> => '_'
}}).

%% API.

start_link(Args) ->
    gen_fsm:start_link(?MODULE, [Args], []).

%% gen_fsm.

init(Args) ->
    [{P1, P2, GameId}] = Args,
    process_flag(trap_exit, true),
    true = gproc:reg({n, l, GameId}),
    {ok, t3_game_state} = dets:open_file(t3_game_state, []),
    State = get_game_state(GameId, P1, P2),
    notify_players(State),
    {ok, State#state.state, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    dets:close(t3_game_state).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

p1_turn({play, P1, Cell}, State = #state{p1 = P1}) ->
    NewState = play(Cell, State, 'O', p2_turn),
    Res = case t3_game:has_won(NewState#state.board, 'O') of
        true ->
            game_won(NewState#state.p1, NewState#state.p2, NewState#state.board),
            {stop, normal, NewState};
        false ->
            case t3_game:is_draw(NewState#state.board) of
                true ->
                    game_drawn(NewState#state.p1, NewState#state.p2, NewState#state.board),
                    {stop, normal, NewState};
                false ->
                    notify_players(NewState),
                    {next_state, p2_turn, NewState}
            end
    end,
    save_game_state(NewState),
    Res.

p2_turn({play, P2, Cell}, State = #state{p2 = P2}) ->
    NewState = play(Cell, State, 'X', p1_turn),
    Res = case t3_game:has_won(NewState#state.board, 'X') of
        true ->
            game_won(NewState#state.p2, NewState#state.p1, NewState#state.board),
            {stop, normal, NewState};
        false ->
            case t3_game:is_draw(NewState#state.board) of
                true ->
                    game_drawn(NewState#state.p1, NewState#state.p2, NewState#state.board),
                    {stop, normal, NewState};
                false ->
                    notify_players(NewState),
                    {next_state, p1_turn, NewState}
            end
    end,
    save_game_state(NewState),
    Res.

play(Cell, State, Symbol, NextState) ->
    '_' = maps:get(Cell, State#state.board),
    State#state{board = maps:update(Cell, Symbol, State#state.board), state = NextState}.

notify_players(State) ->
    case State#state.state of
        p1_turn -> notify_players(State#state.p1, State#state.p2, State#state.board);
        p2_turn -> notify_players(State#state.p2, State#state.p1, State#state.board)
    end.

notify_players(Play, Wait, Board) ->
    Play ! {your_turn, Board},
    Wait ! {wait, Board},
    ok.

game_won(Win, Lose, Board) ->
    Win ! {you_win, Board},
    Lose ! {you_lose, Board},
    ok.

game_drawn(P1, P2, Board) ->
    P1 ! {draw, Board},
    P2 ! {draw, Board},
    ok.

get_game_state(GameId, P1, P2) ->
    case dets:lookup(t3_game_state, GameId) of
        [] ->
            save_game_state(#state{game_id = GameId, state = p1_turn, p1 = P1, p2 = P2});
        [{GameId, State}] ->
            State
    end.

save_game_state(State = #state{game_id = GameId}) ->
    ok = dets:insert(t3_game_state, {GameId, State}).

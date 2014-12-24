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

-record(state, {board=[
    ['_','_','_'],
    ['_','_','_'],
    ['_','_','_']
]}).

%% API.

start_link(Args) ->
    gen_fsm:start_link(?MODULE, [Args], []).

%% gen_fsm.

init(Args) ->
    io:format("New game started: ~p~n", [Args]),
    [{P1, P2}] = Args,
    State = #state{},
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

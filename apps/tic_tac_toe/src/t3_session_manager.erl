-module(t3_session_manager).
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

-record(state, {sessions=#{}}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(new_session, _From, State) ->
    SessionId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    NewState = update_session(SessionId, State),
    {reply, {ok, SessionId}, NewState};

handle_call({validate_session, SessionId}, _From, State) ->
    case session_valid(SessionId, State#state.sessions) of
        false ->
            {reply, {error, invalid_session}, State};
        true ->
            %% sliding expiry window
            NewState = update_session(SessionId, State),
            {reply, ok, NewState}
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

half_an_hour_from_now() ->
    Now = calendar:universal_time(),
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Now) + (30 * 60)).

update_session(SessionId, State) ->
    Expiry = half_an_hour_from_now(),
    prune_expired_sessions(State#state{sessions=maps:put(SessionId, Expiry, State#state.sessions)}).

prune_expired_sessions(State) ->
    SessionIds = maps:keys(State#state.sessions),
    {_, ExpiredSessions} = lists:partition(fun(S) -> session_valid(S, State#state.sessions) end, SessionIds),
    State#state{sessions=maps:without(ExpiredSessions, State#state.sessions)}.

session_valid(SessionId, Sessions) ->
    case maps:find(SessionId, Sessions) of
        error -> false;
        {ok, Expires} ->
            Expires > calendar:universal_time()
    end.

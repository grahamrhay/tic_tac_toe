-module(t3_game).

-export([has_won/2]).
-export([is_draw/1]).

-type symbol() :: 'O' | 'X'.
-spec has_won(map(), symbol()) -> boolean().

%% top row
has_won(#{
    <<"1,1">> := S, <<"1,2">> := S, <<"1,3">> := S,
    <<"2,1">> := _, <<"2,2">> := _, <<"2,3">> := _,
    <<"3,1">> := _, <<"3,2">> := _, <<"3,3">> := _
}, S) ->
    true;

%% middle row
has_won(#{
    <<"1,1">> := _, <<"1,2">> := _, <<"1,3">> := _,
    <<"2,1">> := S, <<"2,2">> := S, <<"2,3">> := S,
    <<"3,1">> := _, <<"3,2">> := _, <<"3,3">> := _
}, S) ->
    true;

%% bottom row
has_won(#{
    <<"1,1">> := _, <<"1,2">> := _, <<"1,3">> := _,
    <<"2,1">> := _, <<"2,2">> := _, <<"2,3">> := _,
    <<"3,1">> := S, <<"3,2">> := S, <<"3,3">> := S
}, S) ->
    true;

%% left column
has_won(#{
    <<"1,1">> := S, <<"1,2">> := _, <<"1,3">> := _,
    <<"2,1">> := S, <<"2,2">> := _, <<"2,3">> := _,
    <<"3,1">> := S, <<"3,2">> := _, <<"3,3">> := _
}, S) ->
    true;

%% middle column
has_won(#{
    <<"1,1">> := _, <<"1,2">> := S, <<"1,3">> := _,
    <<"2,1">> := _, <<"2,2">> := S, <<"2,3">> := _,
    <<"3,1">> := _, <<"3,2">> := S, <<"3,3">> := _
}, S) ->
    true;

%% right column
has_won(#{
    <<"1,1">> := _, <<"1,2">> := _, <<"1,3">> := S,
    <<"2,1">> := _, <<"2,2">> := _, <<"2,3">> := S,
    <<"3,1">> := _, <<"3,2">> := _, <<"3,3">> := S
}, S) ->
    true;

%% left diagonal
has_won(#{
    <<"1,1">> := S, <<"1,2">> := _, <<"1,3">> := _,
    <<"2,1">> := _, <<"2,2">> := S, <<"2,3">> := _,
    <<"3,1">> := _, <<"3,2">> := _, <<"3,3">> := S
}, S) ->
    true;

%% right diagonal
has_won(#{
    <<"1,1">> := _, <<"1,2">> := _, <<"1,3">> := S,
    <<"2,1">> := _, <<"2,2">> := S, <<"2,3">> := _,
    <<"3,1">> := S, <<"3,2">> := _, <<"3,3">> := _
}, S) ->
    true;

%% no winner
has_won(_Board, _) -> false.

-spec is_draw(map()) -> boolean().

is_draw(Board) ->
    lists:all(fun(V) -> V =/= '_' end, maps:values(Board)).
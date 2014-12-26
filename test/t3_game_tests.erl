-module(t3_game_tests).

-include_lib("eunit/include/eunit.hrl").

has_won_top_row_test() ->
    Res = t3_game:has_won(#{
        <<"1,1">> => 'X', <<"1,2">> => 'X', <<"1,3">> => 'X',
        <<"2,1">> => 'O', <<"2,2">> => 'O', <<"2,3">> => '_',
        <<"3,1">> => '_', <<"3,2">> => 'O', <<"3,3">> => '_'
    }, 'X'),
    ?assertEqual(true, Res).

has_won_middle_row_test() ->
    Res = t3_game:has_won(#{
        <<"1,1">> => 'X', <<"1,2">> => 'X', <<"1,3">> => 'O',
        <<"2,1">> => 'O', <<"2,2">> => 'O', <<"2,3">> => 'O',
        <<"3,1">> => '_', <<"3,2">> => 'X', <<"3,3">> => '_'
    }, 'O'),
    ?assertEqual(true, Res).

has_won_bottom_row_test() ->
    Res = t3_game:has_won(#{
        <<"1,1">> => '_', <<"1,2">> => '_', <<"1,3">> => 'O',
        <<"2,1">> => 'O', <<"2,2">> => 'O', <<"2,3">> => '_',
        <<"3,1">> => 'X', <<"3,2">> => 'X', <<"3,3">> => 'X'
    }, 'X'),
    ?assertEqual(true, Res).

has_won_left_column_test() ->
    Res = t3_game:has_won(#{
        <<"1,1">> => 'O', <<"1,2">> => '_', <<"1,3">> => '_',
        <<"2,1">> => 'O', <<"2,2">> => 'X', <<"2,3">> => '_',
        <<"3,1">> => 'O', <<"3,2">> => 'X', <<"3,3">> => 'X'
    }, 'O'),
    ?assertEqual(true, Res).

has_won_middle_column_test() ->
    Res = t3_game:has_won(#{
        <<"1,1">> => '_', <<"1,2">> => 'X', <<"1,3">> => '_',
        <<"2,1">> => 'O', <<"2,2">> => 'X', <<"2,3">> => '_',
        <<"3,1">> => 'O', <<"3,2">> => 'X', <<"3,3">> => 'O'
    }, 'X'),
    ?assertEqual(true, Res).

has_won_right_column_test() ->
    Res = t3_game:has_won(#{
        <<"1,1">> => 'X', <<"1,2">> => '_', <<"1,3">> => 'O',
        <<"2,1">> => '_', <<"2,2">> => 'X', <<"2,3">> => 'O',
        <<"3,1">> => '_', <<"3,2">> => 'X', <<"3,3">> => 'O'
    }, 'O'),
    ?assertEqual(true, Res).

has_won_left_diagonal_test() ->
    Res = t3_game:has_won(#{
        <<"1,1">> => 'X', <<"1,2">> => '_', <<"1,3">> => '_',
        <<"2,1">> => 'O', <<"2,2">> => 'X', <<"2,3">> => '_',
        <<"3,1">> => 'O', <<"3,2">> => 'O', <<"3,3">> => 'X'
    }, 'X'),
    ?assertEqual(true, Res).

has_won_right_diagonal_test() ->
    Res = t3_game:has_won(#{
        <<"1,1">> => 'X', <<"1,2">> => '_', <<"1,3">> => 'O',
        <<"2,1">> => '_', <<"2,2">> => 'O', <<"2,3">> => '_',
        <<"3,1">> => 'O', <<"3,2">> => 'X', <<"3,3">> => 'X'
    }, 'O'),
    ?assertEqual(true, Res).

has_won_no_winner_test() ->
    Res = t3_game:has_won(#{
        <<"1,1">> => 'X', <<"1,2">> => '_', <<"1,3">> => 'O',
        <<"2,1">> => '_', <<"2,2">> => '_', <<"2,3">> => '_',
        <<"3,1">> => 'O', <<"3,2">> => 'X', <<"3,3">> => 'X'
    }, 'O'),
    ?assertEqual(false, Res).

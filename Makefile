PROJECT = tic_tac_toe
DEPS = cowboy jiffy

dep_cowboy = git https://github.com/ninenines/cowboy master
dep_jiffy = git https://github.com/davisp/jiffy.git 0.13.3

include erlang.mk

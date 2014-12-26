PROJECT = tic_tac_toe
DEPS = cowboy jiffy uuid gproc
EUNIT_DIR=test

dep_cowboy = git https://github.com/ninenines/cowboy master
dep_jiffy = git https://github.com/davisp/jiffy.git 0.13.3
dep_uuid = git https://github.com/okeuday/uuid v1.3.3
dep_gproc = git https://github.com/uwiger/gproc.git master

include erlang.mk

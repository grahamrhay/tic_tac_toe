PROJECT = tic_tac_toe
DEPS = cowboy jiffy uuid gproc lager
EUNIT_DIR=test

dep_cowboy = git https://github.com/ninenines/cowboy master
dep_jiffy = git https://github.com/davisp/jiffy.git 0.13.3
dep_uuid = git https://github.com/okeuday/uuid v1.3.3
dep_gproc = git https://github.com/uwiger/gproc.git master
dep_lager = git https://github.com/basho/lager.git 2.1.0

include erlang.mk

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

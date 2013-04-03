
ERL_SOURCES = \
	$(PIQI_ERL_FILES) \

PIQI_FILES = empty_record.piqi defaults.piqi

PRE_TARGET = $(PIQI_ERLANG_FILES)


all: ebin


test:
	cmp defaults_piqi.hrl defaults_piqi.hrl-test


include ../Makefile.piqi
include ../Makefile.erlang


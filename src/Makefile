
all:
	cd ..; rebar compile skip_deps=true


clean:
	cd ..; rebar clean skip_deps=true



ERL_TESTS = piqirun


ERL_APP_PATH = $(PIQI_ROOT)/piqi-erlang
ERLC_FLAGS += -I ../include
#EBIN_DIR = .


PIQIC = piqic
PIQIC_FLAGS = --gen-defaults


PIQI_FILES = piqi-rpc.piqi piqi-tools.piqi piqi.piqi

PIQI_ERL_FILES = piqi_rpc_piqi.erl piqi_tools_piqi.erl piqi_piqi.erl
PIQI_HRL_FILES = piqi_rpc_piqi.hrl piqi_tools_piqi.hrl piqi_piqi.hrl


PRE_TARGET =


VSN = $(shell cat $(PIQI_ROOT)/VERSION)
ERL_APP_FILE = piqi.app



test: beams
	for i in $(ERL_TESTS); do \
	    $(ERL) -pa $(EBIN_DIR) -noshell -s eunit test $$i -s init stop; \
	done


$(ERL_APP_FILE): $(ERL_APP_FILE).src
	sed -e 's/git/"$(VSN)"/' $< > $@


$(PIQI_ERL_FILES) $(PIQI_HRL_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) erlang $(PIQIC_FLAGS) $$i; \
	done


proper:
	touch piqirun_props.erl
	$(MAKE) ERLC_FLAGS='-D PROPER'
	$(ERL) -pa $(EBIN_DIR) -noshell -eval 'proper:module(piqirun_props, 10000)' -s init stop


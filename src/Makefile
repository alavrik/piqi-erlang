
# TODO: uncomment this once the the new (fixed) version of piqi is released
#PIQI = ../priv/bin-"`uname -s`-`uname -m`"/piqi
PIQI = piqi

#PIQIC = ../piqic-erlang/piqic-erlang
PIQIC = ERL_FLAGS="-pa ../piqic-erlang/ebin" ../priv/bin/piqic-erlang
PIQIC_FLAGS = #--trace


all:
	cd ..; rebar compile skip_deps=true


clean:
	cd ..; rebar clean skip_deps=true
	rm -f piqi.piqi


# recompile piqi self-spec into piqi_piqi.{erl,hrl}
reboot: all
	$(PIQI) cc -o piqi.piqi
	$(PIQIC) $(PIQIC_FLAGS) piqi.piqi


# various tests
#
# TODO,XXX: move them to conventional ../test location

ERL = erl
EBIN_DIR = ../ebin

ERL_TESTS = piqirun
ERLC_FLAGS += -I ../include


test:
	set -e; \
	for i in $(ERL_TESTS); do \
	    $(ERL) -pa $(EBIN_DIR) -noshell -s eunit test $$i -s init stop; \
	done


proper:
	touch piqirun_props.erl
	cd ..; rebar -D PROPER compile skip_deps=true
	$(ERL) -pa $(EBIN_DIR) -noshell -eval 'proper:module(piqirun_props, 10000)' -s init stop


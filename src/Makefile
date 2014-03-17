
ifndef PIQI
export PIQI = $(shell echo ../priv/piqi-binary/"`uname -s`-`uname -m`"/piqi)
endif

#PIQIC = ../piqic-erlang/piqic-erlang
PIQIC = ERL_FLAGS="-pa ../piqic-erlang/ebin" ../priv/bin/piqic-erlang
PIQIC_FLAGS = #--trace


.PHONY: all clean test proper reboot piqi piqi-clean


all:
	cd ..; rebar compile skip_deps=true


clean:
	cd ..; rebar clean skip_deps=true
	rm -f piqi.piqi


# piqirun tests
#
# TODO: move them to piqriun.erl or to ../test/

test:
	touch piqirun_tests.erl
	cd ..; rebar -D TEST compile skip_deps=true
	erl -pa ../ebin -noshell -s eunit test piqirun -s init stop


proper:
	touch piqirun_props.erl
	cd ..; rebar -D PROPER compile skip_deps=true
	erl -pa ../ebin -noshell -eval 'proper:module(piqirun_props, 10000)' -s init stop


# recompile piqi self-spec into piqi_piqi.{erl,hrl}
reboot:
	$(PIQI) cc -o piqi.piqi
	$(PIQIC) $(PIQIC_FLAGS) piqi.piqi


#
# make piqi & piqi-clean
#

PIQI_FILES = piqi_rpc.piqi piqi_tools.piqi


PIQI_ERL_FILES = $(PIQI_FILES:%.piqi=%_piqi.erl)
PIQI_HRL_FILES = $(PIQI_FILES:%.piqi=%_piqi.hrl)

PIQI_ERLANG_FILES = $(PIQI_ERL_FILES) $(PIQI_HRL_FILES)


piqi: $(PIQI_ERLANG_FILES)


$(PIQI_ERLANG_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i; \
	done


piqi-clean:
	rm -f $(PIQI_ERLANG_FILES)

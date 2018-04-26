REBAR ?= rebar


.PHONY: all clean test proper piqi


all:
	cd .. && $(REBAR) compile skip_deps=true


clean:
	cd .. && $(REBAR) clean skip_deps=true
	rm -f piqi.piqi


# piqirun tests
#
# TODO: move them to piqriun.erl or to ../test/

test:
	touch piqirun_tests.erl
	cd ..; $(REBAR) -D TEST compile skip_deps=true
	erl -pa ../ebin -noshell -s eunit test piqirun -s init stop


proper:
	touch piqirun_props.erl
	cd ..; $(REBAR) -D PROPER compile skip_deps=true
	erl -pa ../ebin -noshell -eval 'proper:module(piqirun_props, 10000)' -s init stop



export PIQI ?= $(shell echo ../priv/piqi-binary/"`uname -s`-`uname -m`"/piqi)

PIQIC = ERL_FLAGS="-pa ../ebin" ../priv/bin/piqic-erlang
PIQIC_FLAGS = #--trace


PIQI_FILES = piqi_rpc.piqi piqi_tools.piqi


# recompile piqi self-spec into piqi_piqi.{erl,hrl}, same for other PIQI_FILES
piqi:
	$(PIQI) cc -o piqi.piqi
	$(PIQIC) $(PIQIC_FLAGS) --cc -e piqic-erlang piqi.piqi
	set -e; \
	for i in $(PIQI_FILES); do \
		$(PIQIC) $(PIQIC_FLAGS) --gen-protobuf $$i; \
	done

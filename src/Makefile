
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


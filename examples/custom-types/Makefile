REBAR ?= rebar


# only needed for running tests inside the piqi-erlang project
export ERL_LIBS = $(realpath ../lib):$(realpath ../../_build/default/lib)


all:
	$(REBAR) compile


test:


dialyzer: all
	dialyzer deps/piqi/ebin ./ebin


clean:
	$(REBAR) clean skip_deps=true


distclean: clean
	rm -rf ebin _build rebar.lock


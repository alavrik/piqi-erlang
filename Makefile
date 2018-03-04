REBAR ?= rebar


all:
	$(REBAR) compile


deps:
	$(REBAR) get-deps


# build piqic-erlang executable -- called from rebar.config as a pre-compile step
piqic-erlang:
	$(MAKE) -C piqic-erlang
	$(MAKE) priv/bin/piqic-erlang


piqic-erlang-clean:
	$(MAKE) -C piqic-erlang clean


# we need this so that stubs get rebuilt on compiler changes
priv/bin/piqic-erlang: $(wildcard piqic-erlang/ebin/*.beam)
	touch $@


test: all
	$(REBAR) eunit


dialyzer: all
	dialyzer ./ebin


clean:
	$(REBAR) clean


distclean: clean
	$(REBAR) delete-deps
	rm -rf ebin deps


.PHONY: deps piqic-erlang

REBAR ?= rebar3


all:
	$(REBAR) compile


deps:
	$(REBAR) get-deps


test: all
	$(REBAR) eunit


dialyzer: all
	dialyzer ./ebin


clean:
	$(REBAR) clean


distclean: clean piqi-binary-clean
	rm -rf ebin deps _build rebar.lock


.PHONY: deps


# called from rebar.config compile post_hook -- we need this so that stubs get
# rebuilt on compiler changes
priv/bin/piqic-erlang: src/*
	touch $@


# called from rebar.config compile pre_hook to download the pre-built piqi
# executable
piqi-binary:
	./make/get-piqi-binary


# called from 'delete-deps' rebar2 hook, in case of rebar3 triggered only by
# "make distclean", because rebar3 doesn't support 'delete-deps'
piqi-binary-clean:
	rm -rf priv/piqi-binary

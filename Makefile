REBAR ?= rebar


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


distclean: clean
	$(REBAR) delete-deps
	rm -rf ebin deps


.PHONY: deps


# called from rebar.config compile post_hook -- we need this so that stubs get
# rebuilt on compiler changes
priv/bin/piqic-erlang: ebin/*.beam
	touch $@


# called from rebar.config get-deps pre_hook to download the pre-built piqi
# executable
piqi-binary:
	./make/get-piqi-binary


piqi-binary-clean:
	rm -rf priv/piqi-binary

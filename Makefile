REBAR ?= rebar


all:
	$(REBAR) compile


deps:
	$(REBAR) get-deps


# called from rebar.config post_compile hook -- we need this so that stubs get
# rebuilt on compiler changes
priv/bin/piqic-erlang: ebin/*.beam
	touch $@


test: all
	$(REBAR) eunit


dialyzer: all
	dialyzer ./ebin


clean:
	$(REBAR) -r clean


distclean: clean
	$(REBAR) delete-deps
	rm -rf ebin deps


.PHONY: deps

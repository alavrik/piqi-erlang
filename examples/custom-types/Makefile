REBAR ?= rebar

# workaround for a rebar bug
unexport ERL_LIBS


all:
	$(REBAR) compile


dialyzer: all
	dialyzer deps/piqi/ebin ./ebin


clean:
	$(REBAR) clean


distclean: clean
	rm -rf ebin


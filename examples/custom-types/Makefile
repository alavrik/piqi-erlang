REBAR ?= rebar

# workaround for a rebar bug
unexport ERL_LIBS


all:
	$(REBAR) compile


test:


dialyzer: all
	dialyzer deps/piqi/ebin ./ebin


clean:
	$(REBAR) clean skip_deps=true


distclean: clean
	rm -rf ebin


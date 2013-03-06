REBAR ?= rebar


all:
	$(REBAR) compile
	$(MAKE) -C piqic-erlang


deps:
	$(REBAR) get-deps


test: all
	$(REBAR) eunit


dialyzer: all
	dialyzer ./ebin


clean:
	$(REBAR) clean


distclean: clean
	rm -rf ebin deps


.PHONY: deps


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

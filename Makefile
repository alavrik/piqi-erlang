REBAR ?= rebar


all:
	$(REBAR) compile
	$(MAKE) -C piqic-erlang


deps:
	$(REBAR) get-deps


clean:
	$(REBAR) clean


distclean: clean
	rm -rf ebin deps


.PHONY: deps


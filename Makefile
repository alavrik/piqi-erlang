all:
	$(MAKE) -C piqic-erlang
	rebar compile


deps:
	rebar get-deps


clean:
	$(MAKE) -C piqic-erlang clean
	rebar clean


distclean: clean
	rm -rf ebin deps


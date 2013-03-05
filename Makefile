all:
	$(MAKE) -C piqic-erlang
	rebar compile


reboot: all
	cp src/piqi_piqi.erl src/piqi_piqi.hrl piqic-erlang/src


deps:
	rebar get-deps


clean:
	$(MAKE) -C piqic-erlang clean
	rebar clean


distclean: clean
	rm -rf ebin deps


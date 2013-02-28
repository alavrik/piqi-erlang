all:
	rebar compile


deps:
	rebar get-deps


clean:
	rebar clean


distclean: clean
	rm -rf ebin deps


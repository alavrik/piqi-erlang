REBAR ?= rebar


# only needed for running tests inside the piqi-erlang project
export ERL_LIBS = $(realpath ../lib):$(realpath ../../_build/default/lib)


all:
	$(REBAR) compile


test: test_addressbook test_json_xml


test_addressbook:
	./test


test_json_xml:
	./io_json_xml_pb addressbook.piq.pb


dialyzer: all
	dialyzer deps/piqi/ebin ./ebin


clean:
	$(REBAR) clean skip_deps=true
	rm -f addressbook.piq.pb addressbook.pb.piq l1 l2


distclean: clean
	rm -rf ebin _build rebar.lock


.PHONY: test


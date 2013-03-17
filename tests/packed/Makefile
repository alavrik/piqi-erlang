ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	test.erl


PIQI_FILES = packed.piqi packed_nocompat.piqi


all: ebin


test:
	cat packed.piqi | sed -e 's/\.protobuf-packed//' > unpacked.piqi
	cat test-all.piq | sed -e 's/:packed/:unpacked/' > test-all-unpacked.piq

	$(PIQI) convert -t pb test-all.piq
	./test
	cmp test-all.piq.pb test-all.piq.pb.pb

	cp test-all.piq.pb test-all.piq.pb.orig
	$(PIQI) convert -t pb -o test-all.piq.pb test-all-unpacked.piq
	./test
	cmp test-all.piq.pb.orig test-all.piq.pb.pb


clean::
	rm -f test-all.piq.* unpacked.piqi test-all-unpacked.piq


.PHONY: test


include ../Makefile.piqi
include ../Makefile.erlang


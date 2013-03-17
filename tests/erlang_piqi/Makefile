ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	test.erl


PIQI_FILES = piqi.piqi piqobj.piqi


all: piqi.piqi ebin


piqi.piqi:
	$(PIQI) cc > piqi.piqi


test:
	$(PIQI) convert --add-defaults -t pb piqi.piqi
	./test
	cmp *.pb


clean::
	rm -f piqi.piqi piqi.piqi.* t.*


.PHONY: test


include ../Makefile.piqi
include ../Makefile.erlang


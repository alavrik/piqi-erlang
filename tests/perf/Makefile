ERL_SOURCES = \
	$(PIQI_ERL_FILES) \
	test.erl


PIQI_FILES = addressbook.proto.piqi piqi_obj.piqi


all: piqi_obj.piqi ebin


piqi_obj.piqi:
	$(PIQI) cc | sed -e 's/\.module .*//;s/\.code .*//' >$@


test:
	$(PIQI) convert -t pb addressbook.piq

	echo ":piqi_obj/piqi [" > piqi.piq
	cat piqi_obj.piqi >> piqi.piq
	echo "]" >> piqi.piq

	$(PIQI) convert --add-defaults -t pb --no-warnings piqi.piq
	./test 1 1


clean::
	rm -f addressbook.piq.pb piqi.* piqi_obj.*


.PHONY: test


include ../Makefile.piqi
include ../Makefile.erlang


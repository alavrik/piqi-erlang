PIQI_FILES = normalize.piqi nonormalize.piqi
ESCRIPT = escript

all:
	ln -sf normalize.piqi nonormalize.piqi
	$(MAKE) -f Makefile.normalize all;
	$(MAKE) -f Makefile.nonormalize all;
	$(ERLC) normalize_piqi_test.erl

clean::
	rm -f *.pb
	rm -f *.piq
	rm -f *typenames
	rm -f *.beam
	rm -f nonormalize.piqi

test:
# Get the protobuf blob from normalize.erl
	$(ESCRIPT) normalize_piqi_test.beam get_pb

# convert the protobuf to piq
	$(PIQI) convert -f pb --type piqi normalize_piqi_from_erl.pb -o normalize_piqi_from_erl.piq
	$(PIQI) convert -f pb --type piqi nonormalize_piqi_from_erl.pb -o nonormalize_piqi_from_erl.piq

# convert piqi to piq
	$(PIQI) compile normalize.piqi -o normalize.piq 
	$(PIQI) compile nonormalize.piqi -o nonormalize.piq 

# run tests
	./normalize_test.sh

include ../Makefile.piqi
include ../Makefile.erlang

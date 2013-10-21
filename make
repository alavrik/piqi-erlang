#!/bin/sh
#
# this is a "make" wrapper that chooses $MAKE over gmake over make
# (on some platforms, such as FreeBSD, GNU Make is known as gmake)

if [ -n "$MAKE" ]
then
	M="$MAKE"
elif which gmake 2>&1 >/dev/null
then
	M=gmake
else
	M=make
fi


exec $M "$@"

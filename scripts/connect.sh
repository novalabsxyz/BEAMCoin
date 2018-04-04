#!/bin/sh

if [ $# -lt 2 ]; then
		echo "Usage: $0 <name> <multiaddr> [multiaddr [multiaddr [...]]]"
		exit 1
fi

NAME=$1@127.0.0.1
shift
erl -noinput -noshell -pa _build/default/lib/*/ebin -name client-$$@127.0.0.1 -run beamcoin connect $NAME $@ -s init stop

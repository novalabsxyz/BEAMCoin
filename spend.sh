#!/bin/sh

if [ $# -ne 3 ]; then
		echo "Usage: $0 <name> <amount> <recipient>"
		exit 1
fi

erl -noinput -noshell -pa _build/default/lib/*/ebin -name client-$$@127.0.0.1 -run beamcoin spend $1@127.0.0.1 $2 $3 -s init stop

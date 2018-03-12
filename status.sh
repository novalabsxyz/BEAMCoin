#!/bin/sh

if [ $# -ne 1 ]; then
		echo "Usage $0 <name>"
		exit 1
fi

erl -noinput -noshell -pa _build/default/lib/*/ebin -name client-$$@127.0.0.1 -s beamcoin status $1@127.0.0.1 -s init stop

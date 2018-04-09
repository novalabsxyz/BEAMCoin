#!/bin/sh

if [ $# -ne 1 ]; then
		echo "Usage $0 <name>"
		exit 1
fi

erl -name $1@127.0.0.1 -config config/sys.config -pa _build/default/lib/*/ebin -s lager -s beamcoin genesis

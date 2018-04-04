#!/bin/sh

if [ $# -lt 3 ]; then
	echo "Usage $0 <name> <genesis block file> <node address> [node address [node address [...]]]"
	exit 1
fi

if [ ! -f $2 ]; then
	echo "Block file should be a file"
	exit 1
fi

NAME=$1
shift

erl -name $NAME@127.0.0.1 -pa _build/default/lib/*/ebin -s lager -run beamcoin start_link $@

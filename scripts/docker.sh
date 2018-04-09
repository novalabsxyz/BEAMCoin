#!/bin/bash

NAME=${NAME:=satoshi}

echo "Starting BEAMCoin with: "
echo "NAME="$NAME

if [ "${GENESIS:=false}" = "true" ]; then
    echo "starting in genesis mode"

    erl -noinput -noshell -name $NAME@127.0.0.1 -config config/sys.config -pa _build/default/lib/*/ebin -s lager -s beamcoin genesis
else
    PEERS=${PEERS//,/ }
    GENESIS_BLOCK=${GENESIS_BLOCK:=satoshi@127.0.0.1-genesis.block}

    echo "PEERS="$PEERS
    echo "GENESIS_BLOCK="$GENESIS_BLOCK
    echo "starting in miner mode"

    erl -noinput -noshell -name $NAME@127.0.0.1 -config config/sys.config -pa _build/default/lib/*/ebin -s lager -run beamcoin start_link $GENESIS_BLOCK $PEERS
fi

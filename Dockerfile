FROM erlang:20.2

RUN mkdir -p /opt/beamcoin
WORKDIR /opt/beamcoin

## Fetch / Compile deps
ADD rebar.config rebar.config
ADD rebar.lock rebar.lock

RUN rebar3 get-deps
RUN rebar3 compile

## Add / Compule source
ADD src/ src/
RUN rebar3 compile

EXPOSE 8333

CMD erl -noinput -noshell -name satoshi@127.0.0.1 -pa _build/default/lib/*/ebin -s lager -s beamcoin genesis

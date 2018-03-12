-module(beamcoin_sync_handler).

-behavior(libp2p_framed_stream).

-export([init/3, handle_data/3, hexdump/1]).

-record(state, {
          parent :: pid(),
          multiaddr :: undefined | string()
         }).

init(client, Connection, [Parent]) ->
    {_, MultiAddr} = libp2p_connection:addr_info(Connection),
    {ok, #state{parent=Parent, multiaddr=MultiAddr}};
init(server, _Connection, [Path, Parent]) ->
    [Height, Hash] = string:tokens(Path, "/"),
    lager:info("syncing blocks with peer at height ~p and hash ~s", [Height, Hash]),
    {ok, Reply} = beamcoin:get_blocks(Parent, list_to_integer(Height), hex_to_bin(list_to_binary(Hash))),
    {stop, normal, term_to_binary({sync, Reply})}.

handle_data(client, Data, State) ->
    case binary_to_term(Data) of
        {sync, Blocks} ->
            State#state.parent ! {blocks, Blocks, State#state.multiaddr},
            {stop, normal, State}
    end;
handle_data(server, _Data, State=#state{}) ->
    {stop, normal, State}.

-spec hexdump(binary()) -> string().
hexdump(Bin) ->
  lists:flatten([[io_lib:format("~2.16.0b",[X]) || <<X:8>> <= Bin ]]).

hex_to_bin(Hex) ->
  << begin {ok, [V], []} = io_lib:fread("~16u", [X, Y]), <<V:8/integer-little>> end || <<X:8/integer, Y:8/integer>> <= Hex >>.

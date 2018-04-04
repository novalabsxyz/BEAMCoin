%%%-------------------------------------------------------------------
%% @doc
%% == BEAMCoin ==
%% @end
%%%-------------------------------------------------------------------
-module(beamcoin).

-behaviour(gen_server).

%% hard, but not too hard
-define(LIMIT, math:pow(2, 232)).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/1
    ,genesis/0
    ,status/1
    ,get_blocks/3
    ,spend/1
    ,connect/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
]).

-record(coinbase_txn, {
    payee :: libp2p_crypto:address()
    ,amount :: pos_integer()
}).

-record(payment_txn, {
    payer :: libp2p_crypto:address()
    ,payee :: libp2p_crypto:address()
    ,amount :: pos_integer()
    ,nonce :: non_neg_integer()
    ,signature :: binary()
}).

-type hash() :: <<_:256>>. %% SHA256 digest
-type transaction() :: #coinbase_txn{} | #payment_txn{}.

-record(block, {
    prev_hash :: hash()
    ,height = 0 :: non_neg_integer()
    ,transactions = [] :: [transaction()]
    ,magic = <<>> :: binary()
}).

-record(ledger_entry, {
    nonce = 0 :: non_neg_integer()
    ,balance = 0 :: non_neg_integer()
}).

-type ledger() :: #{libp2p_crypto:address() => #ledger_entry{}}.

-record(blockchain, {
    genesis_hash :: hash()
    ,blocks = #{} :: #{hash() => #block{}}
    ,ledger = #{} :: ledger()
    ,head :: hash()
}).

-record(state, {
    blockchain :: #blockchain{}
    ,address :: libp2p_crypto:address()
    ,swarm :: pid()
    ,miner :: pid()
    ,mempool = [] :: [#payment_txn{}]
}).

-include_lib("public_key/include/public_key.hrl").

-type block() :: #block{}.
-type private_key() :: #'ECPrivateKey'{}.
-type public_key() :: {#'ECPoint'{}, {namedCurve, ?secp256r1}}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_blocks(Pid, Height, Hash) ->
    gen_server:call(Pid, {get_blocks, Height, Hash}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
status([Node]) ->
    pong = net_adm:ping(Node),
    {ok, State} = gen_server:call({?MODULE, Node}, status),
    CurrentHead = maps:get(State#state.blockchain#blockchain.head, State#state.blockchain#blockchain.blocks),
    io:format("blockchain is of height ~p with head ~s~n", [CurrentHead#block.height, beamcoin_sync_handler:hexdump(hash_block(CurrentHead))]),
    io:format("listen addresses are ~s~n", [lists:join(" ", libp2p_swarm:listen_addrs(State#state.swarm))]),
    io:format("miner address is ~s~n", [libp2p_crypto:address_to_b58(State#state.address)]),
    io:format("ledger ~n"),
    riak_core_console_table:print(
        [{address, 50}, {balance, 10}, {nonce, 6}]
        ,[[libp2p_crypto:address_to_b58(Address), Balance, Nonce] || {Address, #ledger_entry{nonce=Nonce, balance=Balance}} <- maps:to_list(State#state.blockchain#blockchain.ledger)]
    ),
    io:format("peers ~n"),
    Peers = libp2p_peerbook:values(libp2p_swarm:peerbook(State#state.swarm)),
    Rows = [[libp2p_crypto:address_to_b58(libp2p_peer:address(Peer)), lists:join("\n", libp2p_peer:listen_addrs(Peer)), lists:join("\n", [libp2p_crypto:address_to_b58(P) || P <- libp2p_peer:connected_peers(Peer)]), erlang:system_time(seconds) - libp2p_peer:timestamp(Peer) ] || Peer <- Peers],
    riak_core_console_table:print([{address, 50}, {'listening on', 30}, {peers, 50}, {age, 8}], Rows),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
connect([NodeStr|MultiAddrs]) ->
    Node = erlang:list_to_atom(NodeStr),
    pong = net_adm:ping(Node),
    gen_server:cast({?MODULE, Node}, {connect, MultiAddrs}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
spend([Node, Amount, Recipient]) ->
    Address = libp2p_crypto:b58_to_address(Recipient),
    {ok, Txn} = gen_server:call({?MODULE, erlang:list_to_atom(Node)}, {spend, erlang:list_to_integer(Amount), Address}),
    io:format("transaction ~p submitted ~n", [Txn]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
genesis() ->
    Name = erlang:node(),
    {_PrivKey, PubKey} = load_keys(Name),
    Address = libp2p_crypto:pubkey_to_address(PubKey),
    CoinBase = #coinbase_txn{payee=Address, amount=reward_amount(0)},
    NewBlock = #block{prev_hash = <<0:256>>, height=0, transactions=[CoinBase]},
    {ok, _Pid} = start_miner(NewBlock, self()),
    receive
        {mined_block, Block, self} ->
            Block
    end,
    file:write_file(erlang:atom_to_list(Name) ++ "-genesis.block", erlang:term_to_binary(Block)),
    lager:info("genesis block ~p", [Block]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Block, []], []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_link([Filename | SeedNodes]=Args) ->
    case file:read_file(Filename) of
        {ok, <<>>} ->
            lager:warning("fail to start ~p retrying in 5s", ["empty file"]),
            timer:sleep(5000),
            ?MODULE:start_link(Args);
        {ok, Bin} ->
            try erlang:binary_to_term(Bin) of
                GenesisBlock ->
                    case erlang:is_record(GenesisBlock, block) of
                        'true' ->
                            gen_server:start_link({local, ?MODULE}, ?MODULE, [GenesisBlock, SeedNodes], []);
                        _ ->
                            lager:warning("fail to start ~p retrying in 5s", ["not block record"]),
                            timer:sleep(5000),
                            ?MODULE:start_link(Args)
                    end
            catch
                Error ->
                    lager:warning("fail to start ~p retrying in 5s", [Error]),
                    timer:sleep(5000),
                    ?MODULE:start_link(Args)
            end;
        Error ->
            lager:warning("fail to start ~p retrying in 5s", [Error]),
            timer:sleep(5000),
            ?MODULE:start_link(Args)
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([GenesisBlock, SeedNodes]) ->
    application:ensure_all_started(ranch),

    % Create swarm / connect to peers
    Name = erlang:node(),
    {PrivKey, PubKey} = load_keys(Name),
    Swarm = start_swarm_server(Name, SeedNodes, {PrivKey, PubKey}),

    GenesisHash = hash_block(GenesisBlock),
    %% add any transactions in the genesis block to our ledger
    {ok, Ledger} = absorb_transactions(GenesisBlock#block.transactions, #{}),
    Blockchain = #blockchain{
        genesis_hash=GenesisHash
        ,blocks=#{GenesisHash => GenesisBlock}
        ,ledger=Ledger
        ,head=GenesisHash
    },

    {ok, PubKey, _} = libp2p_swarm:keys(Swarm),
    Address = libp2p_crypto:pubkey_to_address(PubKey),
    State = #state{
        blockchain=Blockchain
        ,swarm=Swarm
        ,address=Address
    },

    self() ! {start_mining, GenesisBlock},

    {ok, State}.

handle_call({get_blocks, _Height, _Hash}, _From, State) ->
    CurrentHead = maps:get(State#state.blockchain#blockchain.head, State#state.blockchain#blockchain.blocks),
    {ok, Blocks} = parent_blocks([CurrentHead], State#state.blockchain),
    {reply, {ok, Blocks}, State};
handle_call(status, _From, State) ->
    {reply, {ok, State}, State};
handle_call({spend, Amount, Recipient}, _From, State) ->
    #ledger_entry{nonce=Nonce} = maps:get(State#state.address, State#state.blockchain#blockchain.ledger, #ledger_entry{}),
    Txn0 = #payment_txn{payer=State#state.address, payee=Recipient, amount=Amount, nonce=Nonce+1, signature= <<>>},
    {PrivKey, _PubKey} = libp2p_swarm:keys(State#state.swarm),
    Signature = public_key:sign(term_to_binary(Txn0), sha256, PrivKey),
    Txn = Txn0#payment_txn{signature=Signature},
    {reply, {ok, base64:encode(term_to_binary(Txn))}, State#state{mempool=[Txn|State#state.mempool]}};
handle_call(_Msg, _From, State) ->
    lager:warning("unhandled call ~p", [_Msg]),
    {reply, ok, State}.

handle_cast({connect, MultiAddrs}, State) ->
    ok = connect_seed_nodes(MultiAddrs, State#state.swarm),
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:warning("unhandled cast ~p", [_Msg]),
    {noreply, State}.

handle_info({start_mining, Block}, #state{swarm=Swarm}=State) ->
    {ok, Miner} = start_mining(Block, Swarm, []),
    {noreply, State#state{miner=Miner}};
handle_info({mined_block, NewBlock, Addr}, State) ->
    CurrentHead = maps:get(State#state.blockchain#blockchain.head, State#state.blockchain#blockchain.blocks),
    case validate_chain(NewBlock, State#state.blockchain) of
        {error, {missing_block, _Hash}} when Addr /= self ->
            Path = "beamcoin_sync/1.0.0/" ++ erlang:integer_to_list(CurrentHead#block.height) ++ "/" ++ beamcoin_sync_handler:hexdump(hash_block(CurrentHead)),
            case libp2p_swarm:dial(State#state.swarm, Addr, Path) of
                {ok, Conn} ->
                    libp2p_framed_stream:client(beamcoin_sync_handler, Conn, [self()]);
                Other ->
                    lager:notice("Failed to dial sync service on ~p : ~p", [Addr, Other])
            end,
            {noreply, State};
        {error, Error} ->
            lager:info("block error ~p ~p", [NewBlock, Error]),
            {noreply, State};
        {NewLedger, ProposedHead} when CurrentHead#block.height < ProposedHead#block.height ->
            case get_miner(ProposedHead) == State#state.address of
                true ->
                    lager:info("mined a new block!");
                false ->
                    lager:info("received a new block!"),
                    erlang:unlink(State#state.miner),
                    erlang:exit(State#state.miner, kill)
            end,
            lager:info("Head is now ~w", [beamcoin_sync_handler:hexdump(hash_block(ProposedHead))]),
            catch [ M ! {block, ProposedHead} || M <- pg2:get_members(self())],
            Mempool = State#state.mempool -- ProposedHead#block.transactions,
            {ok, Miner} = start_mining(ProposedHead, State#state.swarm, Mempool),
            Blockchain = State#state.blockchain,
            NewHash = hash_block(ProposedHead),
            Blocks = maps:put(NewHash, ProposedHead, Blockchain#blockchain.blocks),
            {noreply, State#state{miner=Miner, mempool=Mempool, blockchain=Blockchain#blockchain{ledger=NewLedger, head=NewHash, blocks=Blocks}}};
        {_NewLedger, _} ->
            case get_miner(NewBlock) == State#state.address andalso Addr == self of
                true ->
                    lager:debug("mined sibling block, ignoring"),
                    {ok, Miner} = start_mining(NewBlock, State#state.swarm, State#state.mempool),
                    {noreply, State#state{miner=Miner}};
                false ->
                    lager:debug("received sibling block, ignoring"),
                    {noreply, State}
            end
    end;
handle_info({blocks, Blocks, From}, State) ->
    %% speculatively add the blocks to our blockchain and see what we get
    NewChain = add_blocks(Blocks, State#state.blockchain),
    %% Assume the blocks are in ascending order
    CurrentHead = maps:get(State#state.blockchain#blockchain.head, State#state.blockchain#blockchain.blocks),
    case validate_chain(lists:last(Blocks), NewChain) of
        {error, Reason} ->
            lager:warning("block sync with ~p failed: ~p", [From, Reason]),
            {noreply, State};
        {NewLedger, ProposedHead} when CurrentHead#block.height < ProposedHead#block.height ->
            lager:info("received a block sync from ~p!", [From]),
            unlink(State#state.miner),
            exit(State#state.miner, kill),
            lager:info("Head is now ~w", [hash_block(ProposedHead)]),
            catch [ M ! {block, ProposedHead} || M <- pg2:get_members(self())],
            Mempool = State#state.mempool -- lists:flatten([ Transactions || #block{transactions=Transactions} <- Blocks]),
            {ok, Miner} = start_mining(ProposedHead, State#state.swarm, Mempool),
            NewHash = hash_block(ProposedHead),
            {noreply, State#state{miner=Miner, mempool=Mempool, blockchain=NewChain#blockchain{ledger=NewLedger, head=NewHash}}};
        {_NewLedger, _} ->
            lager:info("got a stale block sync with ~p", [From]),
            %% send the peer our current head, in response, so they can know to sync with us
            %% TODO ideally we'd not have to broadcast it
            catch [ M ! {block, CurrentHead} || M <- pg2:get_members(self())],
            %% the blocks might be useful, stash them since we know they're valid
            Blockchain = State#state.blockchain,
            {noreply, State#state{blockchain=Blockchain#blockchain{blocks=NewChain#blockchain.blocks}}}
    end;
handle_info(_Msg, State) ->
    lager:warning("unhandled info message ~p", [_Msg]),
    {noreply, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_swarm_server(atom(), [string(), ...], {private_key(), public_key()}) -> pid().
start_swarm_server(Name, [], {PrivKey, PubKey}) ->
    Port = os:getenv("PORT", "0"),

    Opts = [
        {key, {PubKey, libp2p_crypto:mk_sig_fun(PrivKey)}}
    ],
    {ok, Swarm} = libp2p_swarm:start(Name, Opts),
    lager:info("started swarm ~p with ~p", [Swarm, Opts]),

    ok = libp2p_swarm:add_stream_handler(Swarm, "beamcoin/1.0.0", {libp2p_framed_stream, server, [beamcoin_handler, self()]}),
    ok = libp2p_swarm:add_stream_handler(Swarm, "beamcoin_sync/1.0.0", {libp2p_framed_stream, server, [beamcoin_sync_handler, self()]}),
    ok = libp2p_swarm:listen(Swarm, "/ip4/0.0.0.0/tcp/" ++ Port),
    ok = libp2p_swarm:listen(Swarm, "/ip6/::/tcp/" ++ Port),

    ok = pg2:create(self()),
    Swarm;
start_swarm_server(Name, SeedNodes, {PrivKey, PubKey}) ->
    Port = os:getenv("PORT", "0"),

    Opts = [
        {key, {PubKey, libp2p_crypto:mk_sig_fun(PrivKey)}}
        ,{libp2p_group_gossip, [
            {stream_clients, [
                {"beamcoin/1.0.0", {beamcoin_handler, [self()]}}
            ]}
            ,{seed_nodes, SeedNodes}
        ]}
    ],
    {ok, Swarm} = libp2p_swarm:start(Name, Opts),
    lager:info("started swarm ~p with ~p", [Swarm, Opts]),

    ok = libp2p_swarm:add_stream_handler(Swarm, "beamcoin/1.0.0", {libp2p_framed_stream, server, [beamcoin_handler, self()]}),
    ok = libp2p_swarm:add_stream_handler(Swarm, "beamcoin_sync/1.0.0", {libp2p_framed_stream, server, [beamcoin_sync_handler, self()]}),
    ok = libp2p_swarm:listen(Swarm, "/ip4/0.0.0.0/tcp/" ++ Port),
    ok = libp2p_swarm:listen(Swarm, "/ip6/::/tcp/" ++ Port),

    ok = pg2:create(self()),
    Swarm.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_keys(atom()) -> {private_key(), public_key()}.
load_keys(Name) ->
    ok = filelib:ensure_dir("keys/"),
    KeyFile = "keys/" ++ erlang:atom_to_list(Name) ++ ".pem",
    case libp2p_crypto:load_keys(KeyFile) of
        {ok, PrivKey, PubKey} -> {PrivKey, PubKey};
        {error, _} ->
            Keys = {PrivKey, PubKey} = libp2p_crypto:generate_keys(),
            ok = libp2p_crypto:save_keys(Keys, KeyFile),
            {PrivKey, PubKey}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
add_blocks([], Blockchain) ->
    Blockchain;
add_blocks([Block|Tail], Blockchain=#blockchain{blocks=Blocks}) ->
    add_blocks(Tail, Blockchain#blockchain{blocks=maps:put(hash_block(Block), Block, Blocks)}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_miner(#block{transactions=[#coinbase_txn{payee=Account}|_]}) ->
    Account.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec connect_seed_nodes(pid(), [string(), ...]) -> ok.
connect_seed_nodes(_Swarm, []) ->
    ok;
connect_seed_nodes(Swarm, [Node|Tail]) ->
    case libp2p_swarm:dial(Swarm, Node, "beamcoin/1.0.0") of
        {ok, Conn} ->
            libp2p_framed_stream:client(beamcoin_handler, Conn, [self()]);
        _ ->
            ok
    end,
    connect_seed_nodes(Tail, Swarm).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec absorb_transactions([transaction(), ...], ledger()) -> {ok, ledger()} | {error, bad_transaction | bad_signature}.
absorb_transactions([], Ledger) ->
    {ok, Ledger};
absorb_transactions([#coinbase_txn{payee=Address, amount=Amount}|Tail], Ledger) ->
    absorb_transactions(Tail, credit_account(Address, Amount, Ledger));
absorb_transactions([#payment_txn{amount=Amount}|_Tail], _Ledger) when Amount =< 0 ->
    {error, bad_transaction};
absorb_transactions([Txn=#payment_txn{payer=Payer, payee=Payee, amount=Amount, nonce=Nonce, signature=Sig}|Tail], Ledger) ->
    PubKey = libp2p_crypto:address_to_pubkey(Payer),
    case public_key:verify(term_to_binary(Txn#payment_txn{signature= <<>>}), sha256, Sig, PubKey) of
        true ->
            case credit_account(Payee, Amount, debit_account(Payer, Amount, Nonce, Ledger)) of
                error ->
                    {error, bad_transaction};
                NewLedger ->
                    absorb_transactions(Tail, NewLedger)
            end;
        false ->
            {error, bad_signature}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
credit_account(_Address, _Amount, error) ->
    error;
credit_account(Address, Amount, Ledger) ->
    Entry = maps:get(Address, Ledger, #ledger_entry{}),
    maps:put(Address, Entry#ledger_entry{balance = Entry#ledger_entry.balance + Amount}, Ledger).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
debit_account(Address, Amount, Nonce, Ledger) ->
    Entry = maps:get(Address, Ledger, #ledger_entry{}),
    %% check things look OK
    case Nonce == Entry#ledger_entry.nonce + 1 andalso (Entry#ledger_entry.balance - Amount) >= 0 of
        true ->
            maps:put(Address, Entry#ledger_entry{balance = Entry#ledger_entry.balance - Amount, nonce=Nonce}, Ledger);
        _ ->
            error
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_mining(ParentBlock, Swarm, Mempool) ->
    {ok, PubKey, _} = libp2p_swarm:keys(Swarm),
    Address = libp2p_crypto:pubkey_to_address(PubKey),
    NextHeight = ParentBlock#block.height + 1,
    CoinBase = #coinbase_txn{payee=Address, amount=reward_amount(NextHeight)},
    NewBlock = #block{prev_hash=hash_block(ParentBlock), height=NextHeight, transactions=[CoinBase|lists:reverse(Mempool)]},
    start_miner(NewBlock, self()).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
validate_chain(Block, Blockchain) ->
    %% check the hash is under the limit and there's only one coinbase transaction
    <<I:256/integer-unsigned-little>> = hash_block(Block),
    case I < ?LIMIT of
        false ->
            {error, insufficent_hash};
        true ->
            case length(lists:filter(fun(E) -> is_record(E, coinbase_txn) end, Block#block.transactions)) == 1 of
                true ->
                    %% construct the chain back to the genesis block
                    case parent_blocks([Block], Blockchain) of
                        {error, Reason} ->
                            {error, Reason};
                        {ok, Blocks} ->
                            %% attempt to compute a new ledger for this chain
                            try
                                ValidatedLedger = lists:foldl(fun(_Block, {error, _}=Acc) ->
                                                                      Acc;
                                                                 (ABlock, Ledger) ->
                                                                      case absorb_transactions(ABlock#block.transactions, Ledger) of
                                                                          {ok, NewLedger} -> NewLedger;
                                                                          %% return the ledger till a "bad block" is found
                                                                          _ -> throw({Ledger, ABlock})
                                                                      end
                                                              end, #{}, Blocks),
                                {ValidatedLedger, Block}
                            catch
                                throw:{Ledger, B} -> {Ledger, B}
                            end
                    end;
                _ ->
                    {error, incorrect_coinbase_txn}
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec hash_block(block()) -> binary().
hash_block(Block) ->
    crypto:hash(sha256, erlang:term_to_binary(Block)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parent_blocks([Head|Tail], Blockchain) ->
    Hash = Head#block.prev_hash,
    case maps:find(Hash, Blockchain#blockchain.blocks) of
        error ->
            GenesisHash = Blockchain#blockchain.genesis_hash,
            case hash_block(Head) == GenesisHash of
                true ->
                    {ok, [Head|Tail]};
                false ->
                    {error, {missing_block, Hash}}
            end;
        {ok, PrevBlock} ->
            parent_blocks([PrevBlock, Head|Tail], Blockchain)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Reward amounts start at 2018 and decrement by one every height until they reach 0.
%% This is to make very clear this is a toy blockchain you should not use for a long time.
%% @end
%%--------------------------------------------------------------------
reward_amount(Height) ->
    max(0, 2018 - Height).

%%--------------------------------------------------------------------
%% @doc
%% Mining functions
%% @end
%%--------------------------------------------------------------------
-spec start_miner(block(), pid()) -> {'ok', pid()}.
start_miner(Block, Parent) ->
    {ok, erlang:spawn_link(fun() -> mine(Block, Parent) end)}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
mine(Block, Parent) ->
    <<I:256/integer-unsigned-little>> = hash_block(Block),
    case I < ?LIMIT of
        true ->
            Parent ! {mined_block, Block, self},
            catch [ M ! {block, Block} || M <- pg2:get_members(Parent)];
        false ->
            mine(Block#block{magic=crypto:strong_rand_bytes(16)}, Parent)
    end.

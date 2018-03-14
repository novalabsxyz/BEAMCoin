-module(beamcoin).

-type hash() :: <<_:256>>. %% SHA256 digest

-record(coinbase_txn, {
          payee :: libp2p_crypto:address(),
          amount :: pos_integer()
         }).

-record(payment_txn, {
          payer :: libp2p_crypto:address(),
          payee :: libp2p_crypto:address(),
          amount :: pos_integer(),
          nonce :: non_neg_integer(),
          signature :: binary()
         }).

-type transaction() :: #coinbase_txn{} | #payment_txn{}.

-record(block, {
          prev_hash :: hash(),
          height = 0 :: non_neg_integer(),
          transactions = [] :: [transaction()],
          magic = <<>> :: binary()
         }).

-record(ledger_entry, {
          nonce = 0 :: non_neg_integer(),
          balance = 0 :: non_neg_integer()
         }).

-record(blockchain, {
          genesis_hash :: hash(),
          blocks = #{} :: #{hash() => #block{}},
          ledger = #{} :: #{libp2p_crypto:address() => #ledger_entry{}},
          head :: hash()
         }).

-record(state, {
          blockchain :: #blockchain{},
          address :: libp2p_crypto:address(),
          swarm :: pid(),
          miner :: pid(),
          mempool = [] :: [#payment_txn{}]
         }).

%% hard, but not too hard
-define(LIMIT, math:pow(2, 232)).

-behaviour(gen_server).

%% public API
-export([start_link/1, genesis/0, status/1, get_blocks/3, spend/1, connect/1]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

get_blocks(Pid, Height, Hash) ->
    gen_server:call(Pid, {get_blocks, Height, Hash}).

status([Node]) ->
    pong = net_adm:ping(Node),
    {ok, State} = gen_server:call({?MODULE, Node}, status),
    CurrentHead = maps:get(State#state.blockchain#blockchain.head, State#state.blockchain#blockchain.blocks),
    io:format("Blockchain is of height ~p with head ~s~n", [CurrentHead#block.height, beamcoin_sync_handler:hexdump(hash_block(CurrentHead))]),
    io:format("Listen addresses are ~s~n", [lists:join(" ", libp2p_swarm:listen_addrs(State#state.swarm))]),
    io:format("Miner address is ~s~n", [libp2p_crypto:address_to_b58(State#state.address)]),
    io:format("Ledger~n"),
    riak_core_console_table:print([{address, 50}, {balance, 10}, {nonce, 6}],
                                  [ [libp2p_crypto:address_to_b58(Address), Balance, Nonce] || {Address, #ledger_entry{nonce=Nonce, balance=Balance}} <- maps:to_list(State#state.blockchain#blockchain.ledger)]),
    io:format("Peers~n"),
    Peers = libp2p_peerbook:values(libp2p_swarm:peerbook(State#state.swarm)),
    Rows = [ [libp2p_crypto:address_to_b58(libp2p_peer:address(Peer)), lists:join("\n", libp2p_peer:listen_addrs(Peer)), lists:join("\n", [libp2p_crypto:address_to_b58(P) || P <- libp2p_peer:connected_peers(Peer)]) ] || Peer <- Peers],
    riak_core_console_table:print([{address, 50}, {'listening on', 30}, {peers, 50}], Rows),
    ok.

connect([NodeStr|MultiAddrs]) ->
    Node = list_to_atom(NodeStr),
    pong = net_adm:ping(Node),
    gen_server:cast({?MODULE, Node}, {connect, MultiAddrs}).

genesis() ->
    Name = node(),
    {ok, Swarm} = libp2p_swarm:start(Name),
    {_PrivKey, PubKey} = libp2p_swarm:keys(Swarm),
    Address = libp2p_crypto:pubkey_to_address(PubKey),
    CoinBase = #coinbase_txn{payee=Address, amount=reward_amount(0)},
    NewBlock = #block{prev_hash = <<0:256>>, height=0, transactions=[CoinBase]},
    libp2p_swarm:stop(Swarm),
    start_miner(NewBlock, self()),
    receive
        {mined_block, Block, self} ->
            Block
    end,
    file:write_file(atom_to_list(Name)++"-genesis.block", term_to_binary(Block)),
    lager:info("Genesis block ~p", [Block]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Block, []], []).

spend([Node, Amount, Recipient]) ->
    Address = libp2p_crypto:b58_to_address(Recipient),
    {ok, Txn} = gen_server:call({?MODULE, list_to_atom(Node)}, {spend, list_to_integer(Amount), Address}),
    io:format("Transaction ~p submitted~n", [Txn]),
    ok.

start_link([Filename | SeedNodes]) ->
    {ok, Bin} = file:read_file(Filename),
    GenesisBlock = binary_to_term(Bin),
    true = is_record(GenesisBlock, block),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GenesisBlock, SeedNodes], []).

init([GenesisBlock, SeedNodes]) ->
    Name = node(),
    application:ensure_all_started(ranch),
    {ok, Swarm} = libp2p_swarm:start(Name),
    ok = libp2p_swarm:add_stream_handler(Swarm, "beamcoin/1.0.0", {libp2p_framed_stream, server, [beamcoin_handler, self()]}),
    ok = libp2p_swarm:add_stream_handler(Swarm, "beamcoin_sync/1.0.0", {libp2p_framed_stream, server, [beamcoin_sync_handler, self()]}),
    ok = pg2:create(self()),
    libp2p_swarm:listen(Swarm, "/ip4/0.0.0.0/tcp/0"),
    libp2p_swarm:listen(Swarm, "/ip6/::/tcp/0"),
    connect_seed_nodes(SeedNodes, Swarm),
    %% start mining speculatively
    GenesisHash = hash_block(GenesisBlock),
    %% add any transactions in the genesis block to our ledger
    {ok, Ledger} = absorb_transactions(GenesisBlock#block.transactions, #{}),
    Blockchain = #blockchain{genesis_hash=GenesisHash, blocks=#{GenesisHash => GenesisBlock}, ledger=Ledger, head=GenesisHash},
    {ok, Miner} = start_mining(GenesisBlock, Swarm, []),
    {_PrivKey, PubKey} = libp2p_swarm:keys(Swarm),
    Address = libp2p_crypto:pubkey_to_address(PubKey),
    State = #state{blockchain=Blockchain, swarm=Swarm, miner=Miner, address=Address},
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
    connect_seed_nodes(MultiAddrs, State#state.swarm),
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:warning("unhandled cast ~p", [_Msg]),
    {noreply, State}.

handle_info({mined_block, NewBlock, From}, State) ->
    CurrentHead = maps:get(State#state.blockchain#blockchain.head, State#state.blockchain#blockchain.blocks),
    case validate_chain(NewBlock, State#state.blockchain) of
        {error, {missing_block, _Hash}} when From /= self ->
            case libp2p_swarm:dial(State#state.swarm, From, "beamcoin_sync/1.0.0/"++integer_to_list(CurrentHead#block.height) ++"/"++ beamcoin_sync_handler:hexdump(hash_block(CurrentHead))) of
                {ok, Conn} ->
                    libp2p_framed_stream:client(beamcoin_sync_handler, Conn, [self()]);
                Other ->
                    lager:notice("Failed to dial sync service on ~p : ~p", [From, Other])
            end,
            {noreply, State};
        {error, Error} ->
            lager:info("block error ~p ~p", [NewBlock, Error]),
            {noreply, State};
        NewLedger when CurrentHead#block.height < NewBlock#block.height ->
            case get_miner(NewBlock) == State#state.address of
                true ->
                    lager:info("mined a new block!");
                false ->
                    lager:info("received a new block!"),
                    unlink(State#state.miner),
                    exit(State#state.miner, kill)
            end,
            lager:info("Head is now ~w", [beamcoin_sync_handler:hexdump(hash_block(NewBlock))]),
            catch [ M ! {block, NewBlock} || M <- pg2:get_members(self())],
            Mempool = State#state.mempool -- NewBlock#block.transactions,
            {ok, Miner} = start_mining(NewBlock, State#state.swarm, Mempool),
            Blockchain = State#state.blockchain,
            NewHash = hash_block(NewBlock),
            Blocks = maps:put(NewHash, NewBlock, Blockchain#blockchain.blocks),
            {noreply, State#state{miner=Miner, mempool=Mempool, blockchain=Blockchain#blockchain{ledger=NewLedger, head=NewHash, blocks=Blocks}}};
        _NewLedger ->
            case get_miner(NewBlock) == State#state.address andalso From == self of
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
    ProposedHead = lists:last(Blocks),
    CurrentHead = maps:get(State#state.blockchain#blockchain.head, State#state.blockchain#blockchain.blocks),
    case validate_chain(ProposedHead, NewChain) of
        {error, Reason} ->
            lager:warning("block sync with ~p failed: ~p", [From, Reason]),
            {noreply, State};
        NewLedger when CurrentHead#block.height < ProposedHead#block.height ->
            lager:info("received a block sync from ~p!", [From]),
            unlink(State#state.miner),
            exit(State#state.miner, kill),
            lager:info("Head is now ~w", [hash_block(ProposedHead)]),
            catch [ M ! {block, ProposedHead} || M <- pg2:get_members(self())],
            Mempool = State#state.mempool -- lists:flatten([ Transactions || #block{transactions=Transactions} <- Blocks]),
            {ok, Miner} = start_mining(ProposedHead, State#state.swarm, Mempool),
            NewHash = hash_block(ProposedHead),
            {noreply, State#state{miner=Miner, mempool=Mempool, blockchain=NewChain#blockchain{ledger=NewLedger, head=NewHash}}};
        _NewLedger ->
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

%% internal functions

add_blocks([], Blockchain) ->
    Blockchain;
add_blocks([Block|Tail], Blockchain=#blockchain{blocks=Blocks}) ->
    add_blocks(Tail, Blockchain#blockchain{blocks=maps:put(hash_block(Block), Block, Blocks)}).

get_miner(#block{transactions=[#coinbase_txn{payee=Account}|_]}) ->
    Account.

connect_seed_nodes([], _) ->
    ok;
connect_seed_nodes([H|SeedNodes], Swarm) ->
    case libp2p_swarm:dial(Swarm, H, "beamcoin/1.0.0") of
        {ok, Conn} ->
            libp2p_framed_stream:client(beamcoin_handler, Conn, [self()]);
        _ ->
            ok
    end,
    connect_seed_nodes(SeedNodes, Swarm).

absorb_transactions([], Ledger) ->
    {ok, Ledger};
absorb_transactions([#coinbase_txn{payee=Address, amount=Amount}|Tail], Ledger) ->
    absorb_transactions(Tail, credit_account(Address, Amount, Ledger));
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

credit_account(_Address, _Amount, error) ->
    error;
credit_account(Address, Amount, Ledger) ->
    Entry = maps:get(Address, Ledger, #ledger_entry{}),
    maps:put(Address, Entry#ledger_entry{balance = Entry#ledger_entry.balance + Amount}, Ledger).

debit_account(Address, Amount, Nonce, Ledger) ->
    Entry = maps:get(Address, Ledger, #ledger_entry{}),
    %% check things look OK
    case Nonce == Entry#ledger_entry.nonce + 1 andalso (Entry#ledger_entry.balance - Amount) >= 0 of
        true ->
            maps:put(Address, Entry#ledger_entry{balance = Entry#ledger_entry.balance - Amount, nonce=Nonce}, Ledger);
        _ ->
            error
    end.

start_mining(ParentBlock, Swarm, Mempool) ->
    {_PrivKey, PubKey} = libp2p_swarm:keys(Swarm),
    Address = libp2p_crypto:pubkey_to_address(PubKey),
    NextHeight = ParentBlock#block.height + 1,
    CoinBase = #coinbase_txn{payee=Address, amount=reward_amount(NextHeight)},
    NewBlock = #block{prev_hash=hash_block(ParentBlock), height=NextHeight, transactions=[CoinBase|lists:reverse(Mempool)]},
    start_miner(NewBlock, self()).

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
                            lists:foldl(fun(_Block, {error, _}=Acc) ->
                                                Acc;
                                           (ABlock, Ledger) ->
                                                case absorb_transactions(ABlock#block.transactions, Ledger) of
                                                    {ok, NewLedger} -> NewLedger;
                                                    Other -> Other
                                                end
                                        end, #{}, Blocks)
                    end;
                _ ->
                    {error, incorrect_coinbase_txn}
            end
    end.

hash_block(Block) ->
    crypto:hash(sha256, term_to_binary(Block)).

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

%% Reward amounts start at 2018 and decrement by one every height until they reach 0.
%% This is to make very clear this is a toy blockchain you should not use for a long time.
reward_amount(Height) ->
    max(0, 2018 - Height).

%% Mining functions

start_miner(Block, Parent) ->
    {ok, spawn_link(fun() -> find_magic(Block, Parent) end)}.

find_magic(Block, Parent) ->
    <<I:256/integer-unsigned-little>> = hash_block(Block),
    case I < ?LIMIT of
        true ->
            Parent ! {mined_block, Block, self},
            catch [ M ! {block, Block} || M <- pg2:get_members(Parent)];
        false ->
            find_magic(Block#block{magic=crypto:strong_rand_bytes(16)}, Parent)
    end.

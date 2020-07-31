%%---------------------------------------------------------------------------
%% |
%% Module      :  Mnesia
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Mnesia Database FFI module.
%%
%%---------------------------------------------------------------------------
-module('Mnesia').

-include("../Foreign.hrl").

-export([ addTableCopy/3
        , addTableIndex/2
        , clearTable/1
        , createSchema/1
        , createTable/2
        , delTableCopy/2
        , delTableIndex/2
        , delete/2
        , deleteWithLock/3
        , deleteObject/2
        , deleteObjectWithLock/3
        , deleteTable/1
        , dirtyDeleteObject/2
        , next/2
        , prev/2
        , wread/2
        , writeWith/2
        , write/1
        , writeWithLock/3
        , transaction/1
        , transactionWithRetry/2
        , changeTableAccessMode/2
        , changeTableLoadOrder/2
        , changeTableMajority/2
        , forceLoadTable/1
        , moveTableCopy/3
        , readWithLock/3
        , setMasterNodes/1
        , setMasterNodesWithTable/2
        , syncTransaction/1
        , syncTransactionWithRetries/2
        , activity/2
        , foldl/3
        , foldr/3
        , changeConfig/2
        , dumpTables/1
        , lockRecord/3
        , lockTable/2
        , lockGlobal/3
        , matchObjectWith/3
        , matchObject/1
        , sdelete/2
        , sdeleteObject/1
        , swrite/1
        , transformTable/4
        , subscribe/1
        , unsubscribe/1
        , setDebugLevel/1
        , dirtyMatchObjectWith/2
        , dirtyMatchObject/1
        ]).

addTableCopy(Tab, N, ST) ->
  ?IO(return(mnesia:add_table_copy(Tab, N, toErl(ST)))).

addTableIndex(Tab, I) ->
  ?IO(return(mnesia:add_table_index(Tab, I))).

clearTable(Tab) ->
  ?IO(return(mnesia:clear_table(Tab))).

createSchema(Nodes) ->
  ?IO(case mnesia:create_schema(Nodes) of
        ok -> ok;
        {error, Reason} -> error(Reason)
      end).

createTable(Name, Options) ->
  ?IO(return(mnesia:create_table(Name, parseOpts(maps:to_list(Options), [])))).

delTableCopy(Tab, Node) ->
  ?IO(return(mnesia:del_table_copy(Tab, Node))).

delTableIndex(Tab, I) ->
  ?IO(return(mnesia:delTableIndex(Tab, I))).

delete(Tab, Key) ->
  ?IO(mnesia:delete(Tab, Key, write)).

deleteWithLock(Tab, Key, Lock) ->
  ?IO(mnesia:delete(Tab, Key, toErl(Lock))).

deleteObject(Tab, Rec) ->
  ?IO(mnesia:delete_object(Tab, Rec, write)).

deleteObjectWithLock(Tab, Rec, Lock) ->
  ?IO(mnesia:delete_object(Tab, Rec, toErl(Lock))).

deleteTable(Tab) ->
  ?IO(return(mnesia:delete_table(Tab))).

dirtyDeleteObject(Tab, Rec) ->
  ?IO(mnesia:dirty_delete_object(Tab, Rec)).

next(Tab, Key1) ->
  ?IO(case mnesia:next(Tab, Key1) of
        '$end_of_table' -> {'Nothing'};
        Key2 -> {'Just', Key2}
      end).

prev(Tab, Key1) ->
  ?IO(case mnesia:prev(Tab, Key1) of
        '$end_of_table' -> {'Nothing'};
        Key2 -> {'Just', Key2}
      end).

wread(Tab, Key) ->
  ?IO(mnesia:read(Tab, Key, write)).

writeWith(Tab, Rec) ->
  ?IO(mnesia:write(Tab, Rec, write)).

write(Rec) ->
  ?IO(mnesia:write(Rec)).

writeWithLock(Tab, Rec, Lock) ->
  ?IO(mnesia:write(Tab, Rec, toErl(Lock))).

transaction(Fun) ->
  ?IO(transReturn(mnesia:transaction(Fun))).

transactionWithRetry(Fun, Retries) ->
  ?IO(transReturn(mnesia:transaction(Fun, decRet(Retries)))).

changeTableAccessMode(Table, Acc) ->
  ?IO(return(mnesia:change_table_access_mode(Table, toErl(Acc)))).

changeTableLoadOrder(Table, V) ->
  ?IO(return(mnesia:change_table_load_order(Table, V))).

changeTableMajority(Table, B) ->
  ?IO(return(mnesia:change_table_majority(Table, B))).

forceLoadTable(Table) ->
  ?IO(return(mnesia:forceLoadTable(Table))).

moveTableCopy(Table, From, To) ->
  ?IO(return(mnesia:move_table_copy(Table, From, To))).

readWithLock(Table, K, LockKind) ->
  ?IO((mnesia:read(Table, K, toErl(LockKind)))).

setMasterNodes(Nodes) ->
  ?IO(return(mnesia:set_master_nodes(Nodes))).

setMasterNodesWithTable(Table, Nodes) ->
  ?IO(return(mnesia:set_master_nodes(Table, Nodes))).

syncTransaction(Fun) ->
  ?IO(transReturn(mnesia:sync_transaction(Fun))).

syncTransactionWithRetries(Fun, Retries) ->
  ?IO(transReturn(mnesia:sync_transaction(Fun, decRet(Retries)))).

activity(Activity, Fun) ->
  ?IO((mnesia:activity(decAct(Activity), Fun))).

foldl(Fun, Acc0, Tab) ->
  ?IO(mnesia:foldl(fun(R, Acc) -> (Fun(R))(Acc) end, Acc0, Tab)).

foldr(Fun, Acc0, Tab) ->
  ?IO(mnesia:foldr(fun(R, Acc) -> (Fun(R))(Acc) end, Acc0, Tab)).

changeConfig(ConfigKey, ConfigVal) ->
  ?IO(decConfig(mnesia:change_config(decConfig(ConfigKey), decConfig(ConfigVal)))).

dumpTables(Tabs) ->
  ?IO(return(mnesia:dump_tables(Tabs))).

lockRecord(Table, Key, LockKind) ->
  ?IO(mnesia:lock({record, Table, Key}, toErl(LockKind))).

lockTable(Table, LockKind) ->
  ?IO(mnesia:lock({table, Table}, toErl(LockKind))).

lockGlobal(Key, Nodes, LockKind) ->
  ?IO(mnesia:lock({global, Key, Nodes}, toErl(LockKind))).

matchObjectWith(Table, Pattern, LockKind) ->
  ?IO(mnesia:match_object(Table, Pattern, toErl(LockKind))).

matchObject(Pattern) ->
  ?IO(mnesia:match_object(element(1, Pattern), Pattern, read)).

sdelete(Table, Key) ->
  ?IO(mnesia:s_delete({Table, Key})).

sdeleteObject(Tuple) ->
  ?IO(mnesia:s_delete_object(Tuple)).

swrite(Tuple) ->
  ?IO(mnesia:s_write(Tuple)).

transformTable(Table, Fun, NewA, RecName) ->
  ?IO(return(mnesia:transform_table(Table, Fun, NewA, RecName))).

subscribe(What) ->
  ?IO(decSubRes(mnesia:subscribe(decWhat(What)))).

unsubscribe(What) ->
  ?IO(decSubRes(mnesia:unsubscribe(decWhat(What)))).

setDebugLevel(DebugLevel) ->
  ?IO(encDeubg(mnesia:set_debug_level(decDeubg(DebugLevel)))).

dirtyMatchObjectWith(Table, Pattern) ->
  ?IO((mnesia:dirty_match_object(Table, Pattern))).

dirtyMatchObject(Pattern) ->
  ?IO((mnesia:dirty_match_object(Pattern))).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

parseOpts([{tableType, {'Set'}}|Opts], Acc) ->
  parseOpts(Opts, [{type, set}|Acc]);
parseOpts([{tableType, {'OrderedSet'}}|Opts], Acc) ->
  parseOpts(Opts, [{type, ordered_set}|Acc]);
parseOpts([{tableType, {'Bag'}}|Opts], Acc) ->
  parseOpts(Opts, [{type, bag}|Acc]);
parseOpts([{tableType, {'DuplicateBag'}}|Opts], Acc) ->
  parseOpts(Opts, [{type, duplicate_bag}|Acc]);
parseOpts([{accessMode, {'ReadWrite'}}|Opts], Acc) ->
  parseOpts(Opts, [{access_mode, read_write}|Acc]);
parseOpts([{accessMode, {'ReadOnly'}}|Opts], Acc) ->
  parseOpts(Opts, [{access_mode, read_only}|Acc]);
parseOpts([{attributes, Attrs}|Opts], Acc) ->
  parseOpts(Opts, [{attributes, Attrs}|Acc]);
parseOpts([{discCopies, Nodes}|Opts], Acc) ->
  parseOpts(Opts, [{disc_copies, Nodes}|Acc]);
parseOpts([{discOnlyCopies, Nodes}|Opts], Acc) ->
  parseOpts(Opts, [{disc_only_copies, Nodes}|Acc]);
parseOpts([{index, Attrs}|Opts], Acc) ->
  parseOpts(Opts, [{index, Attrs}|Acc]);
parseOpts([{loadOrder, Order}|Opts], Acc) ->
  parseOpts(Opts, [{load_order, Order}|Acc]);
parseOpts([{majority, Bool}|Opts], Acc) ->
  parseOpts(Opts, [{majority, Bool}|Acc]);
parseOpts([{ramCopies, Nodes}|Opts], Acc) ->
  parseOpts(Opts, [{ram_copies, Nodes}|Acc]);
parseOpts([{recordName, Name}|Opts], Acc) ->
  parseOpts(Opts, [{record_name, Name}|Acc]);
parseOpts([{localContent, Bool}|Opts], Acc) ->
  parseOpts(Opts, [{local_content, Bool}|Acc]);
parseOpts([], Acc) -> Acc.

toErl({'RLock'}) -> read;
toErl({'WLock'}) -> write;
toErl({'StickyWLock'}) -> sticky_write;
toErl({'ReadOnly'}) -> read_only;
toErl({'ReadWrite'}) -> read_write.

return({atomic, ok}) -> ok;
return({aborted, Reason}) -> error(Reason).

transReturn({atomic, V}) -> V;
transReturn({aborted, Reason}) -> error(Reason).

decRet({'MaxTime', V}) -> V;
decRet({'Infinity'}) -> infinity.

decAct({'AsyncDirty'}) -> async_dirty;
decAct({'SyncDirty'}) ->  sync_dirty;
decAct({'Transaction'}) -> transaction;
decAct({'SyncTransaction'}) -> sync_transaction.

decConfig({'ExtraDbNodes'}) -> extra_db_nodes;
decConfig({'DcDumpLimit'}) -> dc_dump_limit;
decConfig({'Nodes', Nodes}) -> Nodes;
decConfig({'Number', N}) -> N;
decConfig({'ok', Config}) -> Config;
decConfig({'error', T}) -> error(T);
decConfig(T) -> T.

decWhat({'WSystem'}) -> system;
decWhat({'WActivity'}) -> activity;
decWhat({'WTable', Table}) -> {'table', Table, simple}.

decSubRes({ok, Node}) -> Node;
decSubRes({error, Reason}) -> error(Reason).

decDeubg({'Dnone'})    -> none;
decDeubg({'Dverbose'}) -> verbose;
decDeubg({'Ddebug'})   -> debug;
decDeubg({'Dtrace'})   -> trace.

encDeubg(none)    -> {'Dnone'};
encDeubg(verbose) -> {'Dverbose'};
encDeubg(debug)   -> {'Ddebug'};
encDeubg(trace)   -> {'Dtrace'}.

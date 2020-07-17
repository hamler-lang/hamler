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
        , write/2
        , writeWithLock/3
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

write(Tab, Rec) ->
  ?IO(mnesia:write(Tab, Rec, write)).

writeWithLock(Tab, Rec, Lock) ->
  ?IO(mnesia:write(Tab, Rec, toErl(Lock))).

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
toErl({'StickyWLock'}) -> sticky_write.

return({atomic, ok}) -> ok;
return({aborted, Reason}) -> error(Reason).


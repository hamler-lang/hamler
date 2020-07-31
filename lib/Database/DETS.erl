%%---------------------------------------------------------------------------
%% |
%% Module      :  DETS
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The DETS FFI module.
%%
%%---------------------------------------------------------------------------
-module('DETS').

-include("../Foreign.hrl").

-export([ delete/2
        , deleteAllObjects/1
        , deleteObject/2
        , first/1
        , fromETS/2
        , info/1
        , insert/2
        , insertNew/2
        , lookup/2
        , member/2
        , next/2
        , toETS/2
        , foldl/3
        , foldr/3
        , infoWith/2
        , isDetsFile/1
        , match/2
        , matchContinuation/1
        , matchWithLimit/3
        , matchDelete/2
        , matchObject/2
        , matchObjectContinuation/1
        , matchObjectWithLimit/3
        , openFile/2
        , pid2Name/1
        , slot/2
        , sync/1
        ]).

-import('Maybe', [maybe/1]).

delete(Name, Key) ->
  ?IO(return(dets:delete(Name, Key))).

deleteAllObjects(Name) ->
  ?IO(return(dets:delete_all_objects(Name))).

deleteObject(Name, Object) ->
  ?IO(return(dets:delete_object(Name, Object))).

first(Name) ->
  ?IO(case dets:first(Name) of
        '$end_of_table' -> ?Nothing;
        Key -> ?Just(Key)
      end).

fromETS(Name, EtsTab) ->
  ?IO(return(dets:from_ets(Name, EtsTab))).

info(Name) ->
  ?IO(case dets:info(Name) of
        undefined -> ?Nothing;
        Info -> ?Just(infoRec(Info, #{}))
      end).

insert(Name, Objects) ->
  ?IO(return(dets:insert(Name, Objects))).

insertNew(Name, Objects) ->
  ?IO(return(dets:insert_new(Name, Objects))).

lookup(Name, Key) ->
  ?IO(return(dets:lookup(Name, Key))).

member(Name, Key) ->
  ?IO(return(dets:member(Name, Key))).

next(Name, Key1) ->
  ?IO(case dets:next(Name, Key1) of
        '$end_of_table' -> ?Nothing;
        Key2 -> ?Just(Key2)
      end).

toETS(Name, EtsTab) ->
  ?IO(return(dets:to_ets(Name, EtsTab))).

foldl(Fun, Acc0, Tab) ->
  ?IO(dets:foldl(fun(R, Acc) -> (Fun(R))(Acc) end, Acc0, Tab)).

foldr(Fun, Acc0, Tab) ->
  ?IO(dets:foldr(fun(R, Acc) -> (Fun(R))(Acc) end, Acc0, Tab)).

infoWith(Name, Item) ->
  ?IO(case dets:info(Name, Item) of
        undefined -> {'Nothing'};
        Info -> {'Just', infoRec(Info, #{})}
      end).

isDetsFile(FilePath) ->
  ?IO(return(dets:is_dets_file(FilePath))).

match(Name, Pattern) ->
  ?IO(return(dets:match(Name, Pattern))).

matchContinuation(Continuation) ->
  ?IO(case dets:match(Continuation) of
        {error, Reason} -> error(Reason);
        {Match, Contin} -> {'Just', {Match, Contin}};
        '$end_of_table' -> {'Nothing'}
      end).

matchWithLimit(Name, Pattern, N) ->
  ?IO(case dets:match(Name, Pattern, N) of
        {error, Reason} -> error(Reason);
        {Match, Contin} -> ?Just({Match, Contin});
        '$end_of_table' -> ?Nothing
      end).

matchDelete(Name, Pattern) ->
  ?IO(return(dets:match_delete(Name, Pattern))).

matchObject(Name, Pattern) ->
  ?IO(return(dets:match_object(Name, Pattern))).

matchObjectContinuation(Continuation) ->
  ?IO(case dets:match_object(Continuation) of
        {error, Reason} -> error(Reason);
        {Match, Contin} -> ?Just({Match, Contin});
        '$end_of_table' -> ?Nothing
      end).

matchObjectWithLimit(Name, Pattern, N) ->
  ?IO(case dets:match_object(Name, Pattern, N) of
        {error, Reason} -> error(Reason);
        {Match, Contin} -> ?Just({Match, Contin});
        '$end_of_table' -> ?Nothing
      end).

openFile(Name, Args) ->
   ?IO(return(dets:open_file(Name, parseOpts(Args, [])))).

pid2Name(Pid) ->
  ?IO(maybe(dets:pid2name(Pid))).

slot(Name, I) ->
  ?IO(case dets:slot(Name, I) of
        {error, Reason} -> error(Reason);
        '$end_of_table' -> ?Nothing;
        Objetcs -> ?Just(Objetcs)
      end).

sync(Name) ->
  ?IO(return(dets:sync(Name))).

return({error, Reason}) -> error(Reason);
return(Res) -> Res.

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

parseOpts([{'AccessRead'}|Opts], Acc) ->
  parseOpts(Opts, [{access, read}|Acc]);
parseOpts([{'AccessReadWrite'}|Opts], Acc) ->
  parseOpts(Opts, [{access, read_write}|Acc]);
parseOpts([{'AutoSaveInfinity'}|Opts], Acc) ->
  parseOpts(Opts, [{auto_save, infinity}|Acc]);
parseOpts([{'AutoSave', Time}|Opts], Acc) ->
  parseOpts(Opts, [{auto_save, Time}|Acc]);
parseOpts([{'EstimatedNoObjects', Time}|Opts], Acc) ->
  parseOpts(Opts, [{estimated_no_objects, Time}|Acc]);
parseOpts([{'File', FilePath}|Opts], Acc) ->
  parseOpts(Opts, [{file, FilePath}|Acc]);
parseOpts([{'MaxNoSlots', Value}|Opts], Acc) ->
  parseOpts(Opts, [{max_no_slots, Value}|Acc]);
parseOpts([{'MinNoSlots', Value}|Opts], Acc) ->
  parseOpts(Opts, [{min_no_slots, Value}|Acc]);
parseOpts([{'Keypos', Value}|Opts], Acc) ->
  parseOpts(Opts, [{keypos, Value}|Acc]);
parseOpts([{'RamFile', Value}|Opts], Acc) ->
  parseOpts(Opts, [{ram_file, Value}|Acc]);
parseOpts([{'RepairForce'}|Opts], Acc) ->
  parseOpts(Opts, [{force}|Acc]);
parseOpts([{'Repair', Value}|Opts], Acc) ->
  parseOpts(Opts, [{repair, Value}|Acc]);
parseOpts([{'Bag'}|Opts], Acc) ->
  parseOpts(Opts, [{type, bag}|Acc]);
parseOpts([{'DuplicateBag'}|Opts], Acc) ->
  parseOpts(Opts, [{type, duplicate_bag}|Acc]);
parseOpts([{'Set'}|Opts], Acc) ->
  parseOpts(Opts, [{type, set}|Acc]);
parseOpts([], Acc) -> Acc.

infoRec([{file_size, Size}|Info], M) ->
  infoRec(Info, M#{fileSize => Size});
infoRec([{filename, Name}|Info], M) ->
  infoRec(Info, M#{filename => Name});
infoRec([{keypos, Pos}|Info], M) ->
  infoRec(Info, M#{keyPos => Pos});
infoRec([{size, Size}|Info], M) ->
  infoRec(Info, M#{size => Size});
infoRec([{type, Type}|Info], M) ->
  infoRec(Info, M#{tableType => Type});
infoRec([_|Info], M) ->
  infoRec(Info, M);
infoRec([], M) -> M.

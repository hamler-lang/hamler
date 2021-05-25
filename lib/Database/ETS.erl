%%---------------------------------------------------------------------------
%% |
%% Module      :  ETS
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The ETS FFI module.
%%
%%---------------------------------------------------------------------------
-module('ETS').

-include("../Foreign.hrl").

-export([ new/2
        , info/1
        , file2tab/1
        , first/1
        , last/1
        , next/2
        , prev/2
        , tab2file/2
        , matchContinuation /1
        , matchWithLimit/3
        , matchObjectContinuation /1
        , matchObjectWithLimit/3
        , slot/2
        , tabfileInfo/1
        , updateElement/4
        , file2tabWithVerify/1
        , foldl/3
        , foldr/3
        ]).

new(Name, Options) when is_atom(Name) ->
  ?IO(ets:new(Name, parseOpts(maps:to_list(Options), []))).

info(Tab) ->
  ?IO(case ets:info(Tab) of
        undefined -> {'Nothing'};
        Info -> {'Just', infoRec(Info, #{})}
      end).

file2tab(Filename) ->
  ?IO(case ets:file2tab(Filename) of
        {ok, Tab} -> Tab;
        {eror, Reason} -> error(Reason)
      end).

file2tabWithVerify(Filename) ->
  ?IO(case ets:file2tab(Filename, {verify, true}) of
        {ok, Tab} -> Tab;
        {eror, Reason} -> error(Reason)
      end).

first(Tab) ->
  ?IO(case ets:first(Tab) of
        '$end_of_table' -> ?Nothing;
        Key -> ?Just(Key)
      end).

last(Tab) ->
  ?IO(case ets:last(Tab) of
        '$end_of_table' -> ?Nothing;
        Key -> ?Just(Key)
      end).

next(Tab, Key1) ->
  ?IO(case ets:next(Tab, Key1) of
        '$end_of_table' -> ?Nothing;
        Key2 -> ?Just(Key2)
      end).

prev(Tab, Key1) ->
  ?IO(case ets:prev(Tab, Key1) of
        '$end_of_table' -> ?Nothing;
        Key2 -> ?Just(Key2)
      end).

tab2file(Tab, Filename) ->
  ?IO(case ets:tab2file(Tab, Filename) of
        ok -> ok;
        {error, Reason} -> error(Reason)
      end).

matchContinuation(Continuation) ->
  ?IO(case ets:match(Continuation) of
        '$end_of_table' -> ?Nothing;
        Key2 -> ?Just(Key2)
      end).

matchWithLimit(Tid, Pattern, Limit) ->
  ?IO(case ets:match(Tid, Pattern, Limit) of
        '$end_of_table' -> ?Nothing;
        Key2 -> ?Just(Key2)
      end).

matchObjectContinuation(Continuation) ->
  ?IO(case ets:match_object(Continuation) of
        '$end_of_table' -> ?Nothing;
        Key2 -> ?Just(Key2)
      end).

matchObjectWithLimit(Tid, Pattern, Limit) ->
  ?IO(case ets:match_object(Tid, Pattern, Limit) of
        '$end_of_table' -> ?Nothing;
        Key2 -> ?Just(Key2)
      end).

slot(Tid, I) ->
  ?IO(case ets:slot(Tid, I) of
        '$end_of_table' -> ?Nothing;
        Key2 -> ?Just(Key2)
      end).

tabfileInfo(FilePath) ->
  ?IO(case ets:tabfile_info(FilePath) of
        {ok, TabInfo} -> TabInfo;
        {error, Reason} -> error(Reason)
      end).

updateElement(Tabid, Key, Pos, Value) ->
  ?IO(ets:update_element(Tabid, Key, {Pos, Value})).

foldl(Fun, Acc0, Tab) ->
  ?IO(ets:foldl(fun(R, Acc) -> (Fun(R))(Acc) end, Acc0, Tab)).

foldr(Fun, Acc0, Tab) ->
  ?IO(ets:foldr(fun(R, Acc) -> (Fun(R))(Acc) end, Acc0, Tab)).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

parseOpts([{tableType, {'Set'}}|Opts], Acc) ->
  parseOpts(Opts, [set|Acc]);
parseOpts([{tableType, {'OrderedSet'}}|Opts], Acc) ->
  parseOpts(Opts, [ordered_set|Acc]);
parseOpts([{tableType, {'Bag'}}|Opts], Acc) ->
  parseOpts(Opts, [bag|Acc]);
parseOpts([{tableType, {'DuplicateBag'}}|Opts], Acc) ->
  parseOpts(Opts, [duplicate_bag|Acc]);
parseOpts([{access, {'Public'}}|Opts], Acc) ->
  parseOpts(Opts, [public|Acc]);
parseOpts([{access, {'Protected'}}|Opts], Acc) ->
  parseOpts(Opts, [protected|Acc]);
parseOpts([{access, {'Private'}}|Opts], Acc) ->
  parseOpts(Opts, [private|Acc]);
parseOpts([{namedTable, true}|Opts], Acc) ->
  parseOpts(Opts, [named_table|Acc]);
parseOpts([{namedTable, false}|Opts], Acc) ->
  parseOpts(Opts, Acc);
parseOpts([{keyPos, Pos}|Opts], Acc) ->
  parseOpts(Opts, [{keypos, Pos}|Acc]);
parseOpts([{heir, {'Nothing'}}|Opts], Acc) ->
  parseOpts(Opts, [{heir, none}|Acc]);
parseOpts([{heir, {'Just', {Pid, Data}}}|Opts], Acc) ->
  parseOpts(Opts, [{heir, Pid, Data}|Acc]);
parseOpts([{compressed, true}|Opts], Acc) ->
  parseOpts(Opts, [compressed|Acc]);
parseOpts([{readConcurrency, Bool}|Opts], Acc) ->
  parseOpts(Opts, [{read_concurrency, Bool}|Acc]);
parseOpts([{writeConcurrency, Bool}|Opts], Acc) ->
  parseOpts(Opts, [{write_concurrency, Bool}|Acc]);
parseOpts([{compressed, false}|Opts], Acc) ->
  parseOpts(Opts, Acc);
parseOpts([{decentralizedCounters, Bool}|Opts], Acc) ->
  parseOpts(Opts, [{decentralized_counters, Bool}|Acc]);
parseOpts([], Acc) -> Acc.

infoRec([{id, Id}|Info], M) ->
  infoRec(Info, M#{id => Id});
infoRec([{name, Name}|Info], M) ->
  infoRec(Info, M#{name => Name});
infoRec([{size, Size}|Info], M) ->
  infoRec(Info, M#{size => Size});
infoRec([{node, Node}|Info], M) ->
  infoRec(Info, M#{node => Node});
infoRec([{memory, Size}|Info], M) ->
  infoRec(Info, M#{memory => Size});
infoRec([{owner, Pid}|Info], M) ->
  infoRec(Info, M#{owner => Pid});
infoRec([{heir, none}|Info], M) ->
  infoRec(Info, M#{heir => {'Nothing'}});
infoRec([{heir, Pid}|Info], M) ->
  infoRec(Info, M#{heir => {'Just', Pid}});
infoRec([{type, set}|Info], M) ->
  infoRec(Info, M#{tableType => {'Set'}});
infoRec([{type, ordered_set}|Info], M) ->
  infoRec(Info, M#{tableType => {'OrderedSet'}});
infoRec([{type, bag}|Info], M) ->
  infoRec(Info, M#{tableType => {'Bag'}});
infoRec([{type, duplicate_bag}|Info], M) ->
  infoRec(Info, M#{tableType => {'DuplicateBag'}});
infoRec([{named_table, Bool}|Info], M) ->
  infoRec(Info, M#{namedTable => Bool});
infoRec([{keypos, Pos}|Info], M) ->
  infoRec(Info, M#{keyPos => Pos});
infoRec([{protection, public}|Info], M) ->
  infoRec(Info, M#{protection => {'Public'}});
infoRec([{protection, protected}|Info], M) ->
  infoRec(Info, M#{protection => {'Protected'}});
infoRec([{protection, private}|Info], M) ->
  infoRec(Info, M#{protection => {'Private'}});
infoRec([{compressed, Bool}|Info], M) ->
  infoRec(Info, M#{compressed => Bool});
infoRec([{read_concurrency, Bool}|Info], M) ->
  infoRec(Info, M#{readConcurrency => Bool});
infoRec([{write_concurrency, Bool}|Info], M) ->
  infoRec(Info, M#{writeConcurrency => Bool});
%%infoRec([{decentralized_counters, Bool}|Info], M) ->
%%  infoRec(Info, M#{decentralizedCounters => Bool});
infoRec([_|Info], M) ->
  infoRec(Info, M);
infoRec([], M) -> M.

%%---------------------------------------------------------------------------
%% |
%% Module      :  ETS
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The ETS FFI Module.
%%
%%---------------------------------------------------------------------------
-module('ETS').

-export([new/2, parseOpts/1]).

-spec(new(string(), map()) -> string()).
new(Table, Options) ->
    io:format("Options: ~p~n", [Options]),
    ets:new(list_to_atom(Table), parseOpts(Options)).

parseOpts(#{access := Access,
            compressed := Compressed,
            keyPos := KeyPos,
            readConcurrency := ConRead,
            ttype := Type,
            writeConcurrency := ConWrite
           }) ->
    [access(Access), ttype(Type), named_table, {keypos, KeyPos},
     {read_concurrency, ConRead}, {write_concurrency, ConWrite},
     {compressed, Compressed}].

%%---------------------------------------------------------------------------
%% Transform Options
%%---------------------------------------------------------------------------

access({'Public'}) -> public;
access({'Protected'}) -> protected;
access({'Private'}) -> private.

ttype({'Set'}) -> set;
ttype({'OrderedSet'}) -> ordered_set;
ttype({'Bag'}) -> bag;
ttype({'DuplicateBag'}) -> duplicate_bag.


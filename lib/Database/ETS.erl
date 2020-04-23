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

-export([ new/2
        , info/1
        ]).

-spec(new(string(), map()) -> ets:tab()).
new(Table, Options) ->
    ets:new(list_to_atom(Table), parseOpts(Options)).

-spec(info(ets:tab()) -> {'Nothing'} | {'Just', map()}).
info(TableId) ->
    case ets:info(TableId) of
        undefined -> {'Nothing'};
        Info -> {'Just', info2Map(Info)}
    end.

parseOpts(#{ttype := Type,
            access := Access,
            keyPos := KeyPos,
            readConcurrency := R,
            writeConcurrency := W,
            compressed := Compressed
           }) ->
    Opts = [ttype(Type),
            access(Access),
            named_table,
            {keypos, KeyPos},
            {read_concurrency, R},
            {write_concurrency, W}
           ],
    if Compressed -> [compressed | Opts];
       true -> Opts
    end.

info2Map(Info) -> info2Map(Info, #{}).

info2Map([{id, Id}|Info], M) ->
    info2Map(Info, M#{id => Id});
info2Map([{read_concurrency, R}|Info], M) ->
    info2Map(Info, M#{readConcurrency => R});
info2Map([{write_concurrency, W}|Info], M) ->
    info2Map(Info, M#{writeConcurrency => W});
info2Map([{compressed, C}|Info], M) ->
    info2Map(Info, M#{compressed => C});
info2Map([{memory, Size}|Info], M) ->
    info2Map(Info, M#{memory => Size});
info2Map([{owner, Pid}|Info], M) ->
    info2Map(Info, M#{owner => Pid});
info2Map([{name, Name}|Info], M) ->
    info2Map(Info, M#{name => Name});
info2Map([{size, Size}|Info], M) ->
    info2Map(Info, M#{size => Size});
info2Map([{node, Node}|Info], M) ->
    info2Map(Info, M#{node => Node});
info2Map([{named_table, Bool}|Info], M) ->
    info2Map(Info, M#{namedTable => Bool});
info2Map([{type, Type}|Info], M) ->
    info2Map(Info, M#{ttype => ttype(Type)});
info2Map([{keypos, Pos}|Info], M) ->
    info2Map(Info, M#{keyPos => Pos});
info2Map([{protection, Access}|Info], M) ->
    info2Map(Info, M#{protection => access(Access)});
info2Map([_|Info], M) ->
    info2Map(Info, M);
info2Map([], M) -> M.

%%---------------------------------------------------------------------------
%% Transform Options
%%---------------------------------------------------------------------------

-compile({inline, [access/1]}).
access({'Public'})    -> public;
access({'Protected'}) -> protected;
access({'Private'})   -> private;

access(public)        -> {'Public'};
access(protected)     -> {'Protected'};
access(private)       -> {'Private'}.

-compile({inline, [ttype/1]}).
ttype({'Set'})          -> set;
ttype({'OrderedSet'})   -> ordered_set;
ttype({'Bag'})          -> bag;
ttype({'DuplicateBag'}) -> duplicate_bag;

ttype(set)              -> {'Set'};
ttype(ordered_set)      -> {'OrderedSet'};
ttype(bag)              -> {'Bag'};
ttype(duplicate_bag)    -> {'DuplicateBag'}.

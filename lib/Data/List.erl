%%---------------------------------------------------------------------------
%% |
%% Module      :  List
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The List FFI module.
%%
%%---------------------------------------------------------------------------
-module('List').

-include("../Foreign/Maybe.hrl").

%% FFI
-export([ append/2
        , cons/2
        , head/1
        , init/1
        , tail/1
        , take/2
        , drop/2
        , slice/3
        , uncons/1
        , zipWith/3
        , zipWith3/4
        ]).

-spec(append(list(), list()) -> list()).
append(L1, L2) -> lists:append(L1, L2).

-spec(cons(any(), list()) -> list()).
cons(H, T) -> [H|T].

-spec(head(list()) -> any()).
head([H|_]) -> H.

-spec(init(list()) -> list()).
init(L) -> lists:sublist(L, length(L) - 1).

-spec(tail(list()) -> list()).
tail([_|T]) -> T.

-spec(take(pos_integer(), list()) -> list()).
take(N, L) -> lists:sublist(L, N).

-spec(drop(pos_integer(), list()) -> list()).
drop(N, L) -> lists:sublist(L, N+1, length(L)).

-spec(slice(pos_integer(), pos_integer(), list()) -> list()).
slice(Start, End, L) ->
    lists:slice(Start, End-Start, L).

uncons([])    -> ?Nothing;
uncons([H|T]) -> ?Just({H, T}).

zipWith(Fun, A, B) -> 
    lists:zipwith(fun(L, R) -> (Fun(L))(R) end, A, B).

zipWith3(Fun, A, B, C) -> 
    lists:zipwith3(fun(L, R, K) -> ((Fun(L))(R))(K) end, A, B, C).
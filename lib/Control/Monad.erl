%%---------------------------------------------------------------------------
%% |
%% Module      :  Monad
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Monad Module.
%%
%%---------------------------------------------------------------------------
-module('Monad').

-export([ pureImpl/1
        , bindImpl/2
        , mapListImpl/2
        , applyListImpl/2
        , bindListImpl/2
        ]).

-type(mapFun() :: fun((A :: any()) -> B :: any())).

-spec(pureImpl(any()) -> any()).
pureImpl(X) -> X.

-spec(bindImpl(any(), mapFun()) -> any()).
bindImpl(X, F) -> F(X).

-spec(mapListImpl(mapFun(), list(any())) -> list(any())).
mapListImpl(F, L) -> lists:map(F, L).

-spec(applyListImpl(list(mapFun()), list(any())) -> list(any())).
applyListImpl(Funs, L) ->
    lists:map(fun(A) ->
                  lists:foldl(
                    fun(F, X) -> F(X) end, A, Funs)
              end, L).

-spec(bindListImpl(list(any()), mapFun()) -> list(any())).
bindListImpl(L, F) -> lists:flatten(lists:map(F, L)).

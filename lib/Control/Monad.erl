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

-export([ bindImpl/2
        , bindListImpl/2
        , pureImpl/1
        , seqio/1
        ]).

-spec(bindImpl(any(), fun((A :: term()) -> B :: term())) -> any()).
bindImpl(X, F) -> F(X).

-spec(bindListImpl(list(term()), fun((term()) -> list(term()))) -> list(term())).
bindListImpl(L, F) ->
    lists:append(lists:map(F, L)).

-spec(pureImpl(any()) -> any()).
pureImpl(X) -> X.

seqio(L) when is_list(L) ->
    L.


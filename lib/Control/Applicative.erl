%%---------------------------------------------------------------------------
%% |
%% Module      :  Applicative
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Applicative FFI module.
%%
%%---------------------------------------------------------------------------
-module('Applicative').

-export([applyListImpl/2]).

-type(mapFun() :: fun((A :: any()) -> B :: any())).

-spec(applyListImpl(list(mapFun()), list(any())) -> list(any())).
applyListImpl(Funs, L) -> [F(X) || X <- L, F <- Funs].

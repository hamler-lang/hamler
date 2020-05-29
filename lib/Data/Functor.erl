%%---------------------------------------------------------------------------
%% |
%% Module      :  Functor
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Functor FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Functor').

-export([mapListImpl/2]).

-type(mapFun() :: fun((A :: any()) -> B :: any())).

-spec(mapListImpl(mapFun(), list(any())) -> list(any())).
mapListImpl(F, L) -> lists:map(F, L).

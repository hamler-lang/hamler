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
%% The Functor FFI.
%%
%%---------------------------------------------------------------------------
-module('Functor').

-export([listMap/2]).

listMap(F, L) -> lists:map(F, L).

%%---------------------------------------------------------------------------
%% |
%% Module      :  Atomics
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Atomics FFI module.
%%
%%---------------------------------------------------------------------------
-module('Atomics').

-include("../Foreign.hrl").

-export([new/2]).

-spec(new(pos_integer(), boolean()) -> atomics:atomics_ref()).
new(Arity, Signed) ->
  ?IO(atomics:new(Arity, [{signed, Signed}])).

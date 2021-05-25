%%---------------------------------------------------------------------------
%% |
%% Module      :  Counters
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Counters FFI module.
%%
%%---------------------------------------------------------------------------
-module('Counters').

-include("../Foreign.hrl").

-export([new/2]).

new(Size, Option) ->
  ?IO(counters:new(Size, parseOpt(Option))).

parseOpt({'Atomics'}) -> [atomics];
parseOpt({'WriteConcurrency'}) -> [write_concurrency].

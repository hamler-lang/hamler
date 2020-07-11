%%---------------------------------------------------------------------------
%% |
%% Module      :  Counters
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
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

new(Size, Options) ->
  ?IO(counters:new(Size, parseOpts(Options, []))).

parseOpts([{'Atomics'}|T], Acc) ->
  parseOpts(T, [atomics|Acc]);
parseOpts([{'WriteConcurrency'}|T], Acc) ->
  parseOpts(T, [write_concurrency|Acc]);
parseOpts([], Acc) -> Acc.

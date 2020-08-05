%%---------------------------------------------------------------------------
%% |
%% Module      :  Maybe
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Maybe module.
%%
%%---------------------------------------------------------------------------
-module('Maybe').

-include("Maybe.hrl").

-export([maybe/1]).

maybe(undefined) -> ?Nothing;
maybe(Val) -> ?Just(Val).

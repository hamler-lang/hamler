%%---------------------------------------------------------------------------
%% |
%% Module      :  ToErl
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The ToErl module.
%%
%%---------------------------------------------------------------------------
-module('ToErl').

-export([toErl/1]).

toErl({'Infinity'}) -> inifinity;
toErl(Timeout) when is_integer(Timeout) -> Timeout.

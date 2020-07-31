%%---------------------------------------------------------------------------
%% |
%% Module      :  Proxy
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Process Proxy FFI.
%%
%%---------------------------------------------------------------------------
-module('Proxy').

-include("../../Foreign.hrl").

-export([wakeup/1]).

wakeup(Fun) -> ?RunIO(Fun).

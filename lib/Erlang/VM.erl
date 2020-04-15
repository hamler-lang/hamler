%%---------------------------------------------------------------------------
%% |
%% Module      :  VM
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Erlang VM Module.
%%
%%---------------------------------------------------------------------------
-module('VM').

-export([otpRelease/0]).

-spec(otpRelease() -> string()).
otpRelease() -> erlang:system_info(otp_release).

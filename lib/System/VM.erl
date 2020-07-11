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
%% The Erlang VM FFI module.
%%
%%---------------------------------------------------------------------------
-module('VM').

-include("../Foreign.hrl").

-compile(no_auto_import).

-export([ memory/0
        , otpRelease/0
        ]).

memory() ->
  ?IO(maps:from_list(erlang:memory())).

-spec(otpRelease() -> string()).
otpRelease() ->
  ?IO(erlang:system_info(otp_release)).


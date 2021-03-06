%%---------------------------------------------------------------------------
%% |
%% Module      :  Function
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Function FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Function').

-export([error/1]).

-compile({no_auto_import,[error/1]}).

error(Reason) -> erlang:error(Reason).

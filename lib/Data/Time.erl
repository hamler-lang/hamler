%%---------------------------------------------------------------------------
%% |
%% Module      :  Time
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Time FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Time').

-export([getCurrentTime/0]).

%% TODO:...
getCurrentTime() -> erlang:timestamp().

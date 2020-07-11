%%---------------------------------------------------------------------------
%% |
%% Module      :  Timer
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Timer FFI module.
%%
%%---------------------------------------------------------------------------
-module('Timer').

-include("../Foreign.hrl").

-export([readTimer/1]).

readTimer(TimerRef) ->
  ?IO(case erlang:read_timer(TimerRef) of
        false -> {'Nothing'};
        Time -> {'Just', Time}
      end).

%% erlang:cancel_timer(TimerRef) -> Result
%% erlang:cancel_timer(TimerRef, Options) -> Result | ok

%% erlang:read_timer(TimerRef) -> Result
%% erlang:read_timer(TimerRef, Options) -> Result | ok

%% erlang:send_after(Time, Dest, Msg) -> TimerRef
%% erlang:send_after(Time, Dest, Msg, Options) -> TimerRef

%% erlang:start_timer(Time, Dest, Msg) -> TimerRef
%% erlang:start_timer(Time, Dest, Msg, Options) -> TimerRef

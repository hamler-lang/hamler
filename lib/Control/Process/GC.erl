%%---------------------------------------------------------------------------
%% |
%% Module      :  GC
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The GC FFI module.
%%
%%---------------------------------------------------------------------------
-module('GC').

-include("../../Foreign.hrl").

-compile({no_auto_import, [hibernate/3]}).

-export([garbageCollectProcWith/2, hibernate/1]).

-define(MOD, 'Control.Process.Proxy').

garbageCollectProcWith(Pid, Options) ->
  ?IO(erlang:garbage_collect(Pid, parseGcOpts(Options))).

parseGcOpts(Options) ->
  [case Opt of {'GcMajor'} -> {type, major};
               {'GcMinor'} -> {type, minor};
               {'GcAsync', RequestId} -> {async, RequestId}
   end || Opt <- Options].

%% erlang:hibernate(Module, Function, Args) -> no_return()
hibernate(Fun) -> ?IO(erlang:hibernate(?MOD, wakeup, [Fun])).


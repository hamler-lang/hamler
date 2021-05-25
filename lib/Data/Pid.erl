%%---------------------------------------------------------------------------
%% |
%% Module      :  Pid
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Pid FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Pid').

-export([eqPidImpl/2, cmpPidImpl/3, showPidImpl/1]).

-spec(eqPidImpl(pid(), pid()) -> boolean()).
eqPidImpl(Pid1, Pid2) -> Pid1 == Pid2.

cmpPidImpl(LT, EQ, GT) ->
  fun(Pid1) ->
      fun(Pid2) ->
          if Pid1 > Pid2 -> GT;
             Pid1 < Pid2 -> LT;
             true -> EQ
          end
      end
  end.

-spec(showPidImpl(pid()) -> string()).
showPidImpl(Pid) -> pid_to_list(Pid).

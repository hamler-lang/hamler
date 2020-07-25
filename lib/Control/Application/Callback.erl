%%---------------------------------------------------------------------------
%% |
%% Module      :  Callback
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Application Callback FFI module.
%%
%%---------------------------------------------------------------------------
-module('Callback').

-include("../../Foreign.hrl").

-export([ start/2
        , pre_stop/1
        , stop/1
        ]).

-record(state, {mod, st}).

start(_StartType, _StartArgs = [Mod]) ->
  try ?RunIO(Mod:start()) of
    Pid when is_pid(Pid) ->
      {ok, Pid, #state{mod = Mod, st = undefined}}
  catch
    error:Reason -> {error, Reason}
  end.

pre_stop(State = #state{mod = Mod, st = St}) ->
  case erlang:function_exported(Mod, preStop, 1) of
    true  -> State#state{st = ?RunIO(Mod:preStop(St))};
    false -> State
  end.

stop(#state{mod = Mod, st = _St}) ->
  case erlang:function_exported(Mod, stop, 1) of
    true  -> ?RunIO(Mod:stop());
    false -> ok
  end.

%% construct(normal) -> {'Normal'};
%% construct({takeover, Node}) -> {'Takeover', Node};
%% construct({failover, Node}) -> {'Failover', Node}.

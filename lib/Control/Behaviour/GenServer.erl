%%---------------------------------------------------------------------------
%% |
%% Module      :  GenServer
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The GenServer FFI module.
%%
%%---------------------------------------------------------------------------
-module('GenServer').

-export([ start/3
        , startLink/3
        , startMonitor/3
        , reply/3
        , noReply/1
        , stop/1
        ]).

-define(MOD, 'Control.Behaviour.GenServer.Proxy').

start(Class, Init, Args) ->
  {ok, Pid} = gen_server:start(?MOD, [Class, Init, Args], []),
  Pid.

startLink(Class, Init, Args) ->
  {ok, Pid} = gen_server:start_link(?MOD, [Class, Init, Args], []),
  Pid.

startMonitor(Class, Init, Args) ->
  {ok, {Pid, Mon}} = gen_server:start_monitor(?MOD, [Class, Init, Args], []),
  {Pid, Mon}.

stop(ServerRef) ->
  gen_server:stop(destruct(ServerRef)).

reply(From, Rep, State) ->
  ok = gen_server:reply(From, Rep), State.

noReply(State) -> State.

-compile({inline, [destruct/1]}).
destruct({'ServerPid', Pid}) -> Pid;
destruct({'ServerRef', Name}) -> Name;
destruct({'ServerRefOn', Name, Node}) -> {Name, Node};
destruct({'ServerGlobal', Name}) -> {global, Name};
destruct({'ServerVia', Module, Name}) -> {via, Module, Name};
destruct({'Infinity'}) -> infinity;
destruct({'Timeout', I}) -> I.

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

-export([ startServer/3
        , startLink/3
        , startMonitor/3
        , stopServer/1
        ]).

-define(MOD, 'Control.Behaviour.GenServer.Proxy').

startServer(Class, Init, Args) ->
  {ok, Pid} = gen_server:start(?MOD, [Class, Init, Args], []),
  Pid.

startLink(Class, Init, Args) ->
  {ok, Pid} = gen_server:start_link(?MOD, [Class, Init, Args], []),
  Pid.

startMonitor(Class, Init, Args) ->
  {ok, {Pid, Mon}} = gen_server:start_monitor(?MOD, [Class, Init, Args], []),
  {Pid, Mon}.

stopServer(ServerRef) ->
  gen_server:stop(destruct(ServerRef)).

-compile({inline, [destruct/1]}).
destruct({'ServerPid', Pid}) -> Pid;
destruct({'ServerRef', Name}) -> Name;
destruct({'ServerRefOn', Name, Node}) -> {Name, Node};
destruct({'ServerGlobal', Name}) -> {global, Name};
destruct({'ServerVia', Module, Name}) -> {via, Module, Name};
destruct({'Infinity'}) -> infinity;
destruct({'Timeout', I}) -> I.

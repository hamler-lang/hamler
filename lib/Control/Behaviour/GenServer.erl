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

-export([ abcast/2
        , abcastTo/3
        , call/3
        , callTimeout/3
        , cast/3
        , multiCall/2
        , multiCallTo/3
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

abcast(Name, Req) ->
  gen_server:abcast(Name, Req), ok.

abcastTo(Nodes, Name, Req) ->
  gen_server:abcast(Nodes, Name, Req), ok.

call(_Unused, ServerRef, Req) ->
  gen_server:call(destruct(ServerRef), Req).

callTimeout(ServerRef, Req, Timeout) ->
  gen_server:call(destruct(ServerRef), Req, destruct(Timeout)).

cast(_Unused, ServerRef, Req) ->
  gen_server:cast(destruct(ServerRef), Req).

multiCall(Name, Req) ->
  gen_server:multi_call(Name, Req).

multiCallTo(Nodes, Name, Req) ->
  gen_server:multi_call(Nodes, Name, Req).

-compile({inline, [destruct/1]}).
destruct({'ServerPid', Pid}) -> Pid;
destruct({'ServerRef', Name}) -> Name;
destruct({'ServerRefOn', Name, Node}) -> {Name, Node};
destruct({'ServerGlobal', Name}) -> {global, Name};
destruct({'ServerVia', Module, Name}) -> {via, Module, Name};
destruct({'Infinity'}) -> infinity;
destruct({'Timeout', I}) -> I.


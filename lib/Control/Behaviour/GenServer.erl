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
        , startServerWithName/4
        , startLink/3
        , startMonitor/3
        , stopServer/1
        ]).

-export([ abcast/2
        , abcastAt/3
        , call/2
        , callTo/2
        , callTimeout/3
        , cast/2
        , castTo/2
        , multiCall/2
        , multiCallAt/3
        , multiCallTimeoutAt/4
        ]).

-define(MOD, 'Control.Behaviour.GenServer.Proxy').

startServer(Class, Init, Args) ->
  {ok, Pid} = gen_server:start(?MOD, [Class, Init, Args], []),
  Pid.

startServerWithName(Class, Name, Init, Args) ->
    {ok, Pid} = gen_server:start(nr(Name), ?MOD, [Class, Init, Args], []),
    Pid.

startLink(Class, Init, Args) ->
  {ok, Pid} = gen_server:start_link(?MOD, [Class, Init, Args], []),
  Pid.

startMonitor(Class, Init, Args) ->
  {ok, {Pid, Mon}} = gen_server:start_monitor(?MOD, [Class, Init, Args], []),
  {Pid, Mon}.

stopServer(ServerRef) ->
  gen_server:stop(ref(ServerRef)).

%% abcast :: ServerName -> req -> Process ()
abcast(Name, Req) ->
  gen_server:abcast(Name, Req).

%% abcastAt :: [Node] -> ServerName -> req -> Process ()
abcastAt(Nodes, Name, Req) ->
  gen_server:abcast(Nodes, Name, Req).

%% call :: ServerRef -> req -> Process rep
call(ServerRef, Req) ->
  gen_server:call(ref(ServerRef), Req).

%% callTo :: Pid -> req -> Process rep
callTo(Pid, Req) ->
  gen_server:call(Pid, Req).

%% callTimeout :: ServerRef -> req -> Timeout -> Process rep
callTimeout(ServerRef, Req, Timeout) ->
  gen_server:call(ref(ServerRef), Req, t(Timeout)).

%% cast :: ServerRef -> req -> Process ()
cast(ServerRef, Req) ->
  gen_server:cast(ref(ServerRef), Req).

%% castTo :: Pid -> req -> Process ()
castTo(Pid, Req) ->
  gen_server:cast(Pid, Req).

%% multiCall :: ServerName -> req -> Process [NodeReply rep]
multiCall(Name, Req) ->
  gen_server:multi_call(Name, Req).

%% multiCallAt :: [Node] -> ServerName -> req -> Process [NodeReply rep]
multiCallAt(Nodes, Name, Req) ->
  gen_server:multi_call(Nodes, Name, Req).

multiCallTimeoutAt(Nodes, Name, Timeout, Req) ->
  gen_server:multi_call(Nodes, Name, t(Timeout), Req).

-compile({inline, [ref/1]}).
ref({'ServerPid', Pid}) -> Pid;
ref({'ServerRef', Name}) -> Name;
ref({'ServerRefAt', Name, Node}) -> {Name, Node};
ref({'ServerRefGlobal', Name}) -> {global, Name}.

t({'Infinity'}) -> infinity;
t({'Timeout', I}) -> I.

nr({'Local', Name}) -> {local, Name};
nr({'Global', Name}) -> {global, Name}.

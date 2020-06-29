%%---------------------------------------------------------------------------
%% |
%% Module      :  GenStatem
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The GenStatem FFI module.
%%
%%---------------------------------------------------------------------------
-module('GenStatem').

-export([ start/3
        , startLink/3
        , startMonitor/3
        , replyTo/2
        , shutdown/3
        , stop/1
        ]).

-define(MOD, 'Control.Behaviour.GenStatem.Proxy').

start(Class, Init, Args) ->
  {ok, Pid} = gen_statem:start(?MOD, [Class, Init, Args], []),
  Pid.

startLink(Class, Init, Args) ->
  {ok, Pid} = gen_statem:start_link(?MOD, [Class, Init, Args], []),
  Pid.

startMonitor(Class, Init, Args) ->
  {ok, {Pid, Mon}} = gen_statem:start_monitor(?MOD, [Class, Init, Args], []),
  {Pid, Mon}.

replyTo(From, Reply) ->
  gen_statem:reply(From, Reply).

shutdown(StatemRef, Reason, Timeout) ->
  gen_statem:stop(ref(StatemRef), Reason, timeout(Timeout)).

stop(StatemRef) ->
  gen_statem:stop(ref(StatemRef)).

ref({'StatemPid', Pid}) -> Pid;
ref({'StatemRef', LocalName}) -> LocalName;
ref({'StatemRefOn', LocalName, Node}) -> {LocalName, Node};
ref({'StatemGlobal', GlobalName}) -> GlobalName;
ref({'StatemVia', Module, ViaName}) -> {via, Module, ViaName}.

timeout({'Infinity'}) -> infinity;
timeout({'Timeout', I}) -> I.

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

-export([ startFSM/3
        , startLink/3
        , startMonitor/3
        , shutdownFSM/3
        , stopFSM/1
        , replyTo/2
        ]).

-export([ call/2
        , callTimeout/3
        , cast/2
        ]).

-define(MOD, 'Control.Behaviour.GenStatem.Proxy').

startFSM(Class, Init, Args) ->
  {ok, Pid} = gen_statem:start(?MOD, [Class, Init, Args], []),
  Pid.

startLink(Class, Init, Args) ->
  {ok, Pid} = gen_statem:start_link(?MOD, [Class, Init, Args], []),
  Pid.

startMonitor(Class, Init, Args) ->
  {ok, {Pid, Mon}} = gen_statem:start_monitor(?MOD, [Class, Init, Args], []),
  {Pid, Mon}.

shutdownFSM(StatemRef, Reason, Timeout) ->
  gen_statem:stop(ref(StatemRef), Reason, timeout(Timeout)).

stopFSM(StatemRef) ->
  gen_statem:stop(ref(StatemRef)).

call(StatemRef, Req) ->
  gen_statem:call(ref(StatemRef), Req).

callTimeout(StatemRef, Req, Timeout) ->
  gen_statem:call(ref(StatemRef), Req, timeout(Timeout)).

cast(StatemRef, Msg) ->
  gen_statem:cast(ref(StatemRef), Msg).

replyTo(From, Reply) ->
  gen_statem:reply(From, Reply).

ref({'ServerPid', Pid}) -> Pid;
ref({'StatemRef', LocalName}) -> LocalName;
ref({'StatemRefAt', LocalName, Node}) -> {LocalName, Node};
ref({'StatemRefGlobal', GlobalName}) -> {global, GlobalName}.

timeout({'Infinity'}) -> infinity;
timeout({'Timeout', I}) -> I.


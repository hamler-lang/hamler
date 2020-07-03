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
        , startWith/4
        , startWithGlobal/4
        , startLink/3
        , startLinkWith/4
        , startLinkWithGlobal/4
        , stop/1
        , stopWith/3
        ]).

-export([ call/2
        , callTo/2
        , callTimeout/3
        , cast/2
        , castTo/2
        , replyTo/2
        ]).

-define(MOD, 'Control.Behaviour.GenStatem.Proxy').

%%---------------------------------------------------------------------------
%% | Start/stop statem
%%---------------------------------------------------------------------------

start(Class, Init, Args) ->
  startRet(gen_statem:start(?MOD, [Class, Init, Args], [])).

startWith(Name, Class, Init, Args) ->
  startRet(gen_statem:start({local, Name}, ?MOD, [Class, Init, Args], [])).

startWithGlobal(Name, Class, Init, Args) ->
  startRet(gen_statem:start({global, Name}, ?MOD, [Class, Init, Args], [])).

startLink(Class, Init, Args) ->
  startRet(gen_statem:start_link(?MOD, [Class, Init, Args], [])).

startLinkWith(Class, Name, Init, Args) ->
  startRet(gen_statem:start_link({local, Name}, ?MOD, [Class, Init, Args], [])).

startLinkWithGlobal(Class, Name, Init, Args) ->
  startRet(gen_statem:start_link({global, Name}, ?MOD, [Class, Init, Args], [])).

stop(ServerRef) ->
  gen_statem:stop(toErl(ServerRef)).

stopWith(ServerRef, ExitReason, Timeout) ->
  gen_statem:stop(toErl(ServerRef), toErl(ExitReason), toErl(Timeout)).

%%---------------------------------------------------------------------------
%% | Statem APIs
%%---------------------------------------------------------------------------

call(StatemRef, Req) ->
  gen_statem:call(toErl(StatemRef), Req).

%% callTo :: Pid -> req -> Process rep
callTo(Pid, Req) ->
  gen_statem:call(Pid, Req).

callTimeout(StatemRef, Req, Timeout) ->
  gen_statem:call(toErl(StatemRef), Req, toErl(Timeout)).

cast(StatemRef, Msg) ->
  gen_statem:cast(toErl(StatemRef), Msg).

%% castTo :: Pid -> req -> Process ()
castTo(Pid, Req) ->
  gen_statem:cast(Pid, Req).

replyTo(From, Reply) ->
  gen_statem:reply(From, Reply).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

startRet({ok, Pid}) -> {'StartOk', Pid};
startRet(ignore) -> {'StartIgnore'};
startRet({error, Reason}) -> {'StartError', Reason}.

-compile({inline, [toErl/1]}).
toErl({'StatemPid', Pid}) -> Pid;
toErl({'StatemRef', Name}) -> Name;
toErl({'StatemRefAt', Name, Node}) -> {Name, Node};
toErl({'StatemRefGlobal', Name}) -> {global, Name};

toErl({'ExitReason', Reason}) -> Reason;
toErl({'ExitNormal'}) -> normal;
toErl({'ExitShutdown'}) -> shutdown;

toErl({'Infinity'}) -> infinity;
toErl({'Timeout', I}) -> I.


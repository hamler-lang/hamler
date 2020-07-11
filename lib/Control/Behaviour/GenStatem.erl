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

-include("../../Foreign.hrl").

-export([ start/3
        , startWith/4
        , startWithGlobal/4
        , startLink/3
        , startLinkWith/4
        , startLinkWithGlobal/4
        , startMonitor/3
        , startMonitorWith/4
        , startMonitorWithGlobal/4
        , supStart/3
        , supStartWith/4
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
  ?IO(retPid(gen_statem:start(?MOD, [Class, Init, Args], []))).

startWith(Name, Class, Init, Args) ->
  ?IO(retPid(gen_statem:start({local, Name}, ?MOD, [Class, Init, Args], []))).

startWithGlobal(Name, Class, Init, Args) ->
  ?IO(retPid(gen_statem:start({global, Name}, ?MOD, [Class, Init, Args], []))).

startLink(Class, Init, Args) ->
  ?IO(retPid(gen_statem:start_link(?MOD, [Class, Init, Args], []))).

startLinkWith(Class, Name, Init, Args) ->
  ?IO(retPid(gen_statem:start_link({local, Name}, ?MOD, [Class, Init, Args], []))).

startLinkWithGlobal(Class, Name, Init, Args) ->
  ?IO(retPid(gen_statem:start_link({global, Name}, ?MOD, [Class, Init, Args], []))).

startMonitor(Class, Init, Args) ->
  ?IO(retPid(gen_statem:start_monitor(?MOD, [Class, Init, Args], []))).

startMonitorWith(Class, Name, Init, Args) ->
  ?IO(retPid(gen_statem:start_monitor({local, Name}, ?MOD, [Class, Init, Args], []))).

startMonitorWithGlobal(Class, Name, Init, Args) ->
  ?IO(retPid(gen_statem:start_monitor({global, Name}, ?MOD, [Class, Init, Args], []))).

supStart(Class, Init, Args) ->
  ?IO(gen_statem:start_link(?MOD, [Class, Init, Args], [])).

supStartWith(Name, Class, Init, Args) ->
  ?IO(gen_statem:start_link({local, Name}, ?MOD, [Class, Init, Args], [])).

stop(ServerRef) ->
  ?IO(gen_statem:stop(toErl(ServerRef))).

stopWith(ServerRef, ExitReason, Timeout) ->
  ?IO(gen_statem:stop(toErl(ServerRef), toErl(ExitReason), toErl(Timeout))).

%%---------------------------------------------------------------------------
%% | Statem APIs
%%---------------------------------------------------------------------------

call(StatemRef, Req) ->
  ?IO(gen_statem:call(toErl(StatemRef), Req)).

%% callTo :: Pid -> req -> Process rep
callTo(Pid, Req) ->
  ?IO(gen_statem:call(Pid, Req)).

callTimeout(StatemRef, Req, Timeout) ->
  ?IO(gen_statem:call(toErl(StatemRef), Req, toErl(Timeout))).

cast(StatemRef, Msg) ->
  ?IO(gen_statem:cast(toErl(StatemRef), Msg)).

%% castTo :: Pid -> req -> Process ()
castTo(Pid, Req) ->
  ?IO(gen_statem:cast(Pid, Req)).

replyTo(From, Reply) ->
  ?IO(gen_statem:reply(From, Reply)).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

retPid({ok, Pid}) -> Pid;
retPid(ignore) -> error(ignore);
retPid({error, Reason}) -> error(Reason).

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

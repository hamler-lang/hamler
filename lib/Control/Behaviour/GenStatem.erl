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

-export([ start/2
        , startWith/3
        , startLink/2
        , startLinkWith/3
        , startMonitor/2
        , startMonitorWith/3
        , stop/1
        , stopRef/1
        , stopWith/3
        ]).

-export([ call/2
        , callRef/2
        , callTo/2
        , callTimeout/3
        , cast/2
        , castRef/2
        , castTo/2
        , replyTo/2
        , unhandled/4
        ]).

-define(MOD, 'Control.Behaviour.GenStatem.Proxy').

%%---------------------------------------------------------------------------
%% | Start/stop statem
%%---------------------------------------------------------------------------

start(Class, Init) ->
  ?IO(retPid(gen_statem:start(?MOD, [Class, Init], []))).

startWith(Name, Class, Init) ->
  ?IO(retPid(gen_statem:start({local, Name}, ?MOD, [Class, Init], []))).

startLink(Class, Init) ->
  ?IO(retPid(gen_statem:start_link(?MOD, [Class, Init], []))).

startLinkWith(Class, Name, Init) ->
  ?IO(retPid(gen_statem:start_link({local, Name}, ?MOD, [Class, Init], []))).

startMonitor(Class, Init) ->
  ?IO(retPid(gen_statem:start_monitor(?MOD, [Class, Init], []))).

startMonitorWith(Class, Name, Init) ->
  ?IO(retPid(gen_statem:start_monitor({local, Name}, ?MOD, [Class, Init], []))).

stop(Name) ->
  ?IO(gen_statem:stop(Name)).

stopRef(ServerRef) ->
  ?IO(gen_statem:stop(toErl(ServerRef))).

stopWith(ServerRef, ExitReason, Timeout) ->
  ?IO(gen_statem:stop(toErl(ServerRef), toErl(ExitReason), toErl(Timeout))).

%%---------------------------------------------------------------------------
%% | Statem APIs
%%---------------------------------------------------------------------------

call(Name, Req) ->
  ?IO(gen_statem:call(Name, Req)).

%% callTo :: Pid -> req -> Process rep
callTo(Pid, Req) ->
  ?IO(gen_statem:call(Pid, Req)).

callRef(StatemRef, Req) ->
  ?IO(gen_statem:call(toErl(StatemRef), Req)).

callTimeout(StatemRef, Req, Timeout) ->
  ?IO(gen_statem:call(toErl(StatemRef), Req, toErl(Timeout))).

cast(Name, Msg) ->
  ?IO(gen_statem:cast(Name, Msg)).

castTo(Pid, Req) ->
  ?IO(gen_statem:cast(Pid, Req)).

castRef(StatemRef, Msg) ->
  ?IO(gen_statem:cast(toErl(StatemRef), Msg)).

replyTo(From, Reply) ->
  ?IO(gen_statem:reply(From, Reply)).

unhandled(_, E, S, D) ->
  ?IO(begin
        io:format("Unhandled event: ~p, state: ~p, data: ~p", [E, S, D]),
        {'Keep', S, D}
      end).

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

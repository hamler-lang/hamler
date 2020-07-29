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
        , stopRef/1
        , stopWith/3
        ]).

-export([ callRef/2
        , callTimeout/3
        , castRef/2
        , sendRequest/2
        , unhandled/4
        ]).

-define(MOD, 'Control.Behaviour.GenStatem.Proxy').

%%---------------------------------------------------------------------------
%% | Start/stop statem
%%---------------------------------------------------------------------------

start(Class, Init) ->
  ?IO(retPid(gen_statem:start(?MOD, [Class, Init], []))).

startWith(Class, Name, Init) ->
  ?IO(retPid(gen_statem:start({local, Name}, ?MOD, [Class, Init], []))).

startLink(Class, Init) ->
  ?IO(retPid(gen_statem:start_link(?MOD, [Class, Init], []))).

startLinkWith(Class, Name, Init) ->
  ?IO(retPid(gen_statem:start_link({local, Name}, ?MOD, [Class, Init], []))).

startMonitor(Class, Init) ->
  ?IO(retPid(gen_statem:start_monitor(?MOD, [Class, Init], []))).

startMonitorWith(Class, Name, Init) ->
  ?IO(retPid(gen_statem:start_monitor({local, Name}, ?MOD, [Class, Init], []))).

stopRef(ServerRef) ->
  ?IO(gen_statem:stop(unwrap(ServerRef))).

stopWith(ServerRef, ExitReason, Timeout) ->
  ?IO(gen_statem:stop(unwrap(ServerRef), unwrap(ExitReason), unwrap(Timeout))).

%%---------------------------------------------------------------------------
%% | Statem APIs
%%---------------------------------------------------------------------------

callRef(StatemRef, Req) ->
  ?IO(gen_statem:call(unwrap(StatemRef), Req)).

callTimeout(StatemRef, Req, Timeout) ->
  ?IO(gen_statem:call(unwrap(StatemRef), Req, unwrap(Timeout))).

castRef(StatemRef, Msg) ->
  ?IO(gen_statem:cast(unwrap(StatemRef), Msg)).

sendRequest(StatemRef, Request) ->
  ?IO(gen_statem:send_request(unwrap(StatemRef), Request)).

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

-compile({inline, [unwrap/1]}).
unwrap({'StatemPid', Pid}) -> Pid;
unwrap({'StatemRef', Name}) -> Name;
unwrap({'StatemRefAt', Name, Node}) -> {Name, Node};
unwrap({'StatemRefGlobal', Name}) -> {global, Name};

unwrap({'ExitReason', Reason}) -> Reason;
unwrap({'ExitNormal'}) -> normal;
unwrap({'ExitShutdown'}) -> shutdown;

unwrap({'Infinity'}) -> infinity;
unwrap({'Timeout', I}) -> I.


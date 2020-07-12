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

-include("../../Foreign.hrl").

-export([ start/2
        , startWith/3
        , startLink/2
        , startLinkWith/3
        , startMonitor/2
        , startMonitorWith/3
        , supStart/2
        , supStartWith/3
        , stop/1
        , stopWith/3
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
        , replyTo/2
        , sendRequest/2
        , waitResponse/2
        ]).

-define(MOD, 'Control.Behaviour.GenServer.Proxy').

%%---------------------------------------------------------------------------
%% | Start/stop server
%%---------------------------------------------------------------------------

start(Class, Init) ->
  ?IO(retPid(gen_server:start(?MOD, [Class, Init], []))).

startWith(Name, Class, Init) ->
  ?IO(retPid(gen_server:start({local, Name}, ?MOD, [Class, Init], []))).

startLink(Class, Init) ->
  ?IO(retPid(gen_server:start_link(?MOD, [Class, Init], []))).

startLinkWith(Class, Name, Init) ->
  ?IO(retPid(gen_server:start_link({local, Name}, ?MOD, [Class, Init], []))).

startMonitor(Class, Init) ->
  ?IO(retPid(gen_server:start_monitor(?MOD, [Class, Init], []))).

startMonitorWith(Class, Name, Init) ->
  ?IO(retPid(gen_server:start_monitor({local, Name}, ?MOD, [Class, Init], []))).

supStart(Class, Init) ->
  ?IO(gen_server:start_link(?MOD, [Class, Init], [])).

supStartWith(Name, Class, Init) ->
  ?IO(gen_server:start_link({local, Name}, ?MOD, [Class, Init], [])).

stop(ServerRef) ->
  ?IO(gen_server:stop(toErl(ServerRef))).

stopWith(ServerRef, ExitReason, Timeout) ->
  ?IO(gen_server:stop(toErl(ServerRef), toErl(ExitReason), toErl(Timeout))).

%%---------------------------------------------------------------------------
%% | GenServer APIs
%%---------------------------------------------------------------------------

%% abcast :: ServerName -> req -> Process ()
abcast(Name, Req) ->
  ?IO(gen_server:abcast(Name, Req)).

%% abcastAt :: [Node] -> ServerName -> req -> Process ()
abcastAt(Nodes, Name, Req) ->
  ?IO(gen_server:abcast(Nodes, Name, Req)).

%% call :: ServerRef -> req -> Process rep
call(ServerRef, Req) ->
  ?IO(gen_server:call(toErl(ServerRef), Req)).

%% callTo :: Pid -> req -> Process rep
callTo(Pid, Req) ->
  ?IO(gen_server:call(Pid, Req)).

%% callTimeout :: ServerRef -> req -> Timeout -> Process rep
callTimeout(ServerRef, Req, Timeout) ->
  ?IO(gen_server:call(toErl(ServerRef), Req, toErl(Timeout))).

%% cast :: ServerRef -> req -> Process ()
cast(ServerRef, Req) ->
  ?IO(gen_server:cast(toErl(ServerRef), Req)).

%% castTo :: Pid -> req -> Process ()
castTo(Pid, Req) ->
  ?IO(gen_server:cast(Pid, Req)).

%% multiCall :: ServerName -> req -> Process [NodeReply rep]
multiCall(Name, Req) ->
  ?IO(gen_server:multi_call(Name, Req)).

%% multiCallAt :: [Node] -> ServerName -> req -> Process [NodeReply rep]
multiCallAt(Nodes, Name, Req) ->
  ?IO(gen_server:multi_call(Nodes, Name, Req)).

multiCallTimeoutAt(Nodes, Name, Timeout, Req) ->
  ?IO(gen_server:multi_call(Nodes, Name, toErl(Timeout), Req)).

replyTo(From, Reply) ->
  ?IO(gen_statem:reply(From, Reply)).

sendRequest(ServerRef, Request) ->
  ?IO(gen_server:send_request(toErl(ServerRef), Request)).

waitResponse(RequestId, Timeout) ->
  ?IO(case gen_server:wait_response(RequestId, toErl(Timeout)) of
        {reply, Reply} -> Reply;
        timeout -> error(timeout);
        {error, Reason} -> error(Reason)
      end).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

retPid({ok, Pid}) -> Pid;
retPid(ignore) -> error(ignore);
retPid({error, Reason}) -> error(Reason).

-compile({inline, [toErl/1]}).
toErl({'ServerPid', Pid}) -> Pid;
toErl({'ServerRef', Name}) -> Name;
toErl({'ServerRefAt', Name, Node}) -> {Name, Node};
toErl({'ServerRefGlobal', Name}) -> {global, Name};

toErl({'ExitReason', Reason}) -> Reason;
toErl({'ExitNormal'}) -> normal;
toErl({'ExitShutdown'}) -> shutdown;

toErl({'Infinity'}) -> infinity;
toErl({'Timeout', I}) -> I.

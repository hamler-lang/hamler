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

start(Class, Init, Args) ->
  retPid(gen_server:start(?MOD, [Class, Init, Args], [])).

startWith(Name, Class, Init, Args) ->
  retPid(gen_server:start({local, Name}, ?MOD, [Class, Init, Args], [])).

startWithGlobal(Name, Class, Init, Args) ->
  retPid(gen_server:start({global, Name}, ?MOD, [Class, Init, Args], [])).

startLink(Class, Init, Args) ->
  retPid(gen_server:start_link(?MOD, [Class, Init, Args], [])).

startLinkWith(Class, Name, Init, Args) ->
  retPid(gen_server:start_link({local, Name}, ?MOD, [Class, Init, Args], [])).

startLinkWithGlobal(Class, Name, Init, Args) ->
  retPid(gen_server:start_link({global, Name}, ?MOD, [Class, Init, Args], [])).

startMonitor(Class, Init, Args) ->
  retPid(gen_server:start_monitor(?MOD, [Class, Init, Args], [])).

startMonitorWith(Class, Name, Init, Args) ->
  retPid(gen_server:start_monitor({local, Name}, ?MOD, [Class, Init, Args], [])).

startMonitorWithGlobal(Class, Name, Init, Args) ->
  retPid(gen_server:start_monitor({global, Name}, ?MOD, [Class, Init, Args], [])).

supStart(Class, Init, Args) ->
  gen_server:start_link(?MOD, [Class, Init, Args], []).

supStartWith(Name, Class, Init, Args) ->
  gen_server:start({local, Name}, ?MOD, [Class, Init, Args], []).

stop(ServerRef) ->
  gen_server:stop(toErl(ServerRef)).

stopWith(ServerRef, ExitReason, Timeout) ->
  gen_server:stop(toErl(ServerRef), toErl(ExitReason), toErl(Timeout)).

%%---------------------------------------------------------------------------
%% | GenServer APIs
%%---------------------------------------------------------------------------

%% abcast :: ServerName -> req -> Process ()
abcast(Name, Req) ->
  gen_server:abcast(Name, Req).

%% abcastAt :: [Node] -> ServerName -> req -> Process ()
abcastAt(Nodes, Name, Req) ->
  gen_server:abcast(Nodes, Name, Req).

%% call :: ServerRef -> req -> Process rep
call(ServerRef, Req) ->
  gen_server:call(toErl(ServerRef), Req).

%% callTo :: Pid -> req -> Process rep
callTo(Pid, Req) ->
  gen_server:call(Pid, Req).

%% callTimeout :: ServerRef -> req -> Timeout -> Process rep
callTimeout(ServerRef, Req, Timeout) ->
  gen_server:call(toErl(ServerRef), Req, toErl(Timeout)).

%% cast :: ServerRef -> req -> Process ()
cast(ServerRef, Req) ->
  gen_server:cast(toErl(ServerRef), Req).

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
  gen_server:multi_call(Nodes, Name, toErl(Timeout), Req).

replyTo(From, Reply) ->
  gen_statem:reply(From, Reply).

sendRequest(ServerRef, Request) ->
  gen_server:send_request(toErl(ServerRef), Request).

waitResponse(RequestId, Timeout) ->
  case gen_server:wait_response(RequestId, toErl(Timeout)) of
    {reply, Reply} -> Reply;
    timeout -> error(timeout);
    {error, Reason} -> error(Reason)
  end.

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

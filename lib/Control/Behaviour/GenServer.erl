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
        , stop/1
        , stopPid/1
        , stopRef/1
        , stopWith/3
        ]).

-export([ abcast/2
        , abcastAt/3
        , call/2
        , callTo/2
        , callRef/2
        , callTimeout/3
        , cast/2
        , castTo/2
        , castRef/2
        , multiCall/2
        , multiCallAt/3
        , multiCallTimeoutAt/4
        , sendRequest/2
        , waitResponse/2
        ]).

-define(MOD, 'Control.Behaviour.GenServer.Proxy').

%%---------------------------------------------------------------------------
%% | Start/stop server
%%---------------------------------------------------------------------------

start(Class, Init) ->
    ?IO(retPid(gen_server:start(?MOD, [Class, Init], []))).

startWith(Class, Name, Init) ->
    ?IO(retPid(gen_server:start({local, Name}, ?MOD, [Class, Init], []))).

startLink(Class, Init) ->
    ?IO(retPid(gen_server:start_link(?MOD, [Class, Init], []))).

startLinkWith(Class, Name, Init) ->
    ?IO(retPid(gen_server:start_link({local, Name}, ?MOD, [Class, Init], []))).

startMonitor(Class, Init) ->
    ?IO(retPid(gen_server:start_monitor(?MOD, [Class, Init], []))).

startMonitorWith(Class, Name, Init) ->
    ?IO(retPid(gen_server:start_monitor({local, Name}, ?MOD, [Class, Init], []))).

stop(Name) ->
    ?IO(gen_server:stop(Name)).

stopPid(Pid) ->
    ?IO(gen_server:stop(Pid)).

stopRef(ServerRef) ->
    ?IO(gen_server:stop(unwrap(ServerRef))).

stopWith(ServerRef, ExitReason, Timeout) ->
    ?IO(gen_server:stop(unwrap(ServerRef), unwrap(ExitReason), unwrap(Timeout))).

%%---------------------------------------------------------------------------
%% | GenServer APIs
%%---------------------------------------------------------------------------

%% abcast :: ServerName -> req -> Process ()
abcast(Name, Req) ->
    ?IO(gen_server:abcast(Name, Req)).

%% abcastAt :: [Node] -> ServerName -> req -> Process ()
abcastAt(Nodes, Name, Req) ->
    ?IO(gen_server:abcast(Nodes, Name, Req)).

%% call :: Name -> req -> Process rep
call(Name, Req) ->
    ?IO(gen_server:call(Name, Req)).

%% callTo :: Pid -> req -> Process rep
callTo(Pid, Req) ->
    ?IO(gen_server:call(Pid, Req)).

%% call :: ServerRef -> req -> Process rep
callRef(ServerRef, Req) ->
    ?IO(gen_server:call(unwrap(ServerRef), Req)).

%% callTimeout :: ServerRef -> req -> Timeout -> Process rep
callTimeout(ServerRef, Req, Timeout) ->
    ?IO(gen_server:call(unwrap(ServerRef), Req, unwrap(Timeout))).

%% cast :: Name -> req -> Process ()
cast(Name, Req) ->
    ?IO(gen_server:cast(Name, Req)).

%% castTo :: Pid -> req -> Process ()
castTo(Pid, Req) ->
    ?IO(gen_server:cast(Pid, Req)).

%% castRef :: ServerRef -> req -> Process ()
castRef(ServerRef, Req) ->
    ?IO(gen_server:cast(unwrap(ServerRef), Req)).

%% multiCall :: ServerName -> req -> Process (Replies rep)
multiCall(Name, Req) ->
    ?IO(replies(gen_server:multi_call(Name, Req))).

%% multiCallAt :: [Node] -> ServerName -> req -> Process [NodeReply rep]
multiCallAt(Nodes, Name, Req) ->
    ?IO(replies(gen_server:multi_call(Nodes, Name, Req))).

multiCallTimeoutAt(Nodes, Name, Timeout, Req) ->
    ?IO(replies(gen_server:multi_call(Nodes, Name, unwrap(Timeout), Req))).

sendRequest(ServerRef, Request) ->
    ?IO(gen_server:send_request(unwrap(ServerRef), Request)).

waitResponse(RequestId, Timeout) ->
  ?IO(case gen_server:wait_response(RequestId, unwrap(Timeout)) of
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

-compile({inline, [unwrap/1]}).
unwrap({'ServerPid', Pid}) -> Pid;
unwrap({'ServerRef', Name}) -> Name;
unwrap({'ServerRefAt', Name, Node}) -> {Name, Node};
unwrap({'ServerRefGlobal', Name}) -> {global, Name};

unwrap({'ExitReason', Reason}) -> Reason;
unwrap({'ExitNormal'}) -> normal;
unwrap({'ExitShutdown'}) -> shutdown;

unwrap({'Infinity'}) -> infinity;
unwrap({'Timeout', I}) -> I.

replies({Replies, BadNodes}) ->
    lists:append([{Node, ?Just(Rep)} || {Node, Rep} <- Replies],
                 [{Node, ?Nothing} || Node <- BadNodes]).


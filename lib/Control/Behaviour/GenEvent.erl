%%---------------------------------------------------------------------------
%% |
%% Module      :  GenEvent
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The GenEvent FFI Module.
%%
%%---------------------------------------------------------------------------
-module('GenEvent').

-export([ startEventMgr/2
        , notify/2
        , syncNotify/2
        , shutdown/3
        , stop/1
        ]).

-define(MOD, 'Control.Behaviour.GenEvent.Proxy').

startEventMgr(Class, Args) ->
    {ok, Pid} = gen_server:start_link(?MOD, [Class, Args], []),
    Pid.

notify(EventMgrRef, Event) ->
    gen_event:notify(ref(EventMgrRef), Event).

syncNotify(EventMgrRef, Event) ->
    gen_event:sync_notify(ref(EventMgrRef), Event).

shutdown(EventMgrRef, Reason, Timeout) ->
    gen_event:stop(ref(EventMgrRef), Reason, timeout(Timeout)).

stop(EventMgrRef) ->
    gen_event:stop(ref(EventMgrRef)).

ref({'EventMgrPid', Pid}) -> Pid;
ref({'EventMgrRef', Name}) -> Name;
ref({'EventMgrRefAt', Name, Node}) -> {Name, Node};
ref({'EventMgrRefGlobal', Name}) -> {global, Name}.

timeout({'Infinity'}) -> infinity;
timeout({'Timeout', I}) -> I.


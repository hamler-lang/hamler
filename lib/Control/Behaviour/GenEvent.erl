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
%% The GenEvent FFI module.
%%
%%---------------------------------------------------------------------------
-module('GenEvent').

-include("../../Foreign.hrl").

-export([ start/2
        , startWith/3
        , startLink/2
        , startLinkWith/3
        , stopRef/1
        , stopWith/3
        ]).

-export([ addHandler/3
        , notifyRef/2
        , syncNotify/2
        , syncNotifyTo/2
        , syncNotifyRef/2
        ]).

-define(MOD, 'Control.Behaviour.GenEvent.Proxy').

%%---------------------------------------------------------------------------
%% | Start/Stop
%%---------------------------------------------------------------------------

start(Class, Init) ->
    ?IO(doStart(fun gen_event:start/0, Class, Init)).

startWith(Class, Name, Init) ->
    ?IO(doStartWith(fun gen_event:start/1, {local, Name}, Class, Init)).

startLink(Class, Init) ->
    ?IO(doStart(fun gen_event:start_link/0, Class, Init)).

startLinkWith(Class, Name, Init) ->
    ?IO(doStartWith(fun gen_event:start_link/1, {local, Name}, Class, Init)).

stopRef(EMgrRef) ->
    ?IO(gen_event:stop(unwrap(EMgrRef))).

stopWith(EMgrRef, Reason, Timeout) ->
    ?IO(apply(gen_event, stop, [unwrap(A) || A <- [EMgrRef, Reason, Timeout]])).

%%---------------------------------------------------------------------------
%% | GenEvent APIs
%%---------------------------------------------------------------------------

addHandler(Class, EMgrRef, Init) ->
    ?IO(case gen_event:add_handler(EMgrRef, {?MOD, Class}, [Class, Init]) of
            ok -> ok;
            {'EXIT', Reason} -> error(Reason)
        end).

notifyRef(EMgrRef, Event) ->
    ?IO(gen_event:notify(unwrap(EMgrRef), Event)).

syncNotify(Name, Event) ->
    ?IO(gen_event:sync_notify(Name, Event)).

syncNotifyTo(Pid, Event) ->
    ?IO(gen_event:sync_notify(Pid, Event)).

syncNotifyRef(EMgrRef, Event) ->
    ?IO(gen_event:sync_notify(unwrap(EMgrRef), Event)).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

doStart(Start, Class, Init) ->
    {ok, Pid} = Start(),
    ok = (addHandler(Class, Pid, Init))(),
    Pid.

doStartWith(Start, Name, Class, Init) ->
    case Start(Name) of
        {ok, Pid} ->
            ok = (addHandler(Class, Pid, Init))(),
            Pid;
        {error, Reason} ->
            error(Reason)
    end.

unwrap({'EMgrPid', Pid}) -> Pid;
unwrap({'EMgrRef', Name}) -> Name;
unwrap({'EMgrRefAt', Name, Node}) -> {Name, Node};
unwrap({'EMgrRefGlobal', Name}) -> {global, Name};

unwrap({'Infinity'}) -> infinity;
unwrap({'Timeout', I}) -> I.


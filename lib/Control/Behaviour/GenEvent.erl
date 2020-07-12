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
        , supStart/2
        , supStartWith/3
        , stop/1
        , stopWith/3
        ]).

-export([ addHandler/3
        , notify/2
        , notifyTo/2
        , syncNotify/2
        , syncNotifyTo/2
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

supStart(Class, Init) ->
  ?IO(doSupStart(fun gen_event:start_link/0, Class, Init)).

supStartWith(Class, Name, Init) ->
  ?IO(doSupStartWith(fun gen_event:start_link/1, {local, Name}, Class, Init)).

stop(EMgrRef) ->
  ?IO(gen_event:stop(toErl(EMgrRef))).

stopWith(EMgrRef, Reason, Timeout) ->
  ?IO(apply(gen_event, stop, [toErl(A) || A <- [EMgrRef, Reason, Timeout]])).

%%---------------------------------------------------------------------------
%% | GenEvent APIs
%%---------------------------------------------------------------------------

addHandler(Class, EMgrRef, Init) ->
  ?IO(case gen_event:add_handler(EMgrRef, {?MOD, Class}, [Class, Init]) of
        ok -> ok;
        {'EXIT', Reason} -> error(Reason)
      end).

notify(EMgrRef, Event) ->
  ?IO(gen_event:notify(toErl(EMgrRef), Event)).

notifyTo(Pid, Event) ->
  ?IO(gen_event:notify(Pid, Event)).

syncNotify(EMgrRef, Event) ->
  ?IO(gen_event:sync_notify(toErl(EMgrRef), Event)).

syncNotifyTo(Pid, Event) ->
  ?IO(gen_event:sync_notify(Pid, Event)).

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

doSupStart(Start, Class, Init) ->
  {ok, Pid} = Start(),
  ok = (addHandler(Class, Pid, Init))(),
  {ok, Pid}.

doSupStartWith(Start, Name, Class, Init) ->
  case Start(Name) of
    {ok, Pid} ->
      ok = (addHandler(Class, Pid, Init))(),
      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

toErl({'EMgrPid', Pid}) -> Pid;
toErl({'EMgrRef', Name}) -> Name;
toErl({'EMgrRefAt', Name, Node}) -> {Name, Node};
toErl({'EMgrRefGlobal', Name}) -> {global, Name};

toErl({'Infinity'}) -> infinity;
toErl({'Timeout', I}) -> I.


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

-export([ start/3
        , startWith/4
        , startWithGlobal/4
        , startLink/3
        , startLinkWith/4
        , startLinkWithGlobal/4
        , supStart/3
        , supStartWith/4
        , stop/1
        , stopWith/3
        ]).

-export([ addHandler/4
        , notify/2
        , notifyTo/2
        , syncNotify/2
        , syncNotifyTo/2
        ]).

-define(MOD, 'Control.Behaviour.GenEvent.Proxy').

%%---------------------------------------------------------------------------
%% | Start/Stop
%%---------------------------------------------------------------------------

start(Class, Init, Args) ->
  ?IO(doStart(fun gen_event:start/0, Class, Init, Args)).

startWith(Class, Name, Init, Args) ->
  ?IO(doStartWith(fun gen_event:start/1, {local, Name}, Class, Init, Args)).

startWithGlobal(Class, Name, Init, Args) ->
  ?IO(doStartWith(fun gen_event:start/1, {global, Name}, Class, Init, Args)).

startLink(Class, Init, Args) ->
  ?IO(doStart(fun gen_event:start_link/0, Class, Init, Args)).

startLinkWith(Class, Name, Init, Args) ->
  ?IO(doStartWith(fun gen_event:start_link/1, {local, Name}, Class, Init, Args)).

startLinkWithGlobal(Class, Name, Init, Args) ->
  ?IO(doStartWith(fun gen_event:start_link/1, {global, Name}, Class, Init, Args)).

supStart(Class, Init, Args) ->
  ?IO(doSupStart(fun gen_event:start_link/0, Class, Init, Args)).

supStartWith(Class, Name, Init, Args) ->
  ?IO(doSupStartWith(fun gen_event:start_link/1, {local, Name}, Class, Init, Args)).

stop(EMgrRef) ->
  ?IO(gen_event:stop(toErl(EMgrRef))).

stopWith(EMgrRef, Reason, Timeout) ->
  ?IO(apply(gen_event, stop, [toErl(A) || A <- [EMgrRef, Reason, Timeout]])).

%%---------------------------------------------------------------------------
%% | GenEvent APIs
%%---------------------------------------------------------------------------

addHandler(Class, EMgrRef, Init, Args) ->
  ?IO(case gen_event:add_handler(EMgrRef, {?MOD, Class}, [Class, Init, Args]) of
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

doStart(Start, Class, Init, Args) ->
  {ok, Pid} = Start(),
  ok = addHandler(Class, Pid, Init, Args),
  Pid.

doStartWith(Start, Name, Class, Init, Args) ->
  case Start(Name) of
    {ok, Pid} ->
      ok = addHandler(Class, Pid, Init, Args),
      Pid;
    {error, Reason} ->
      error(Reason)
  end.

doSupStart(Start, Class, Init, Args) ->
  {ok, Pid} = Start(),
  ok = addHandler(Class, Pid, Init, Args),
  {ok, Pid}.

doSupStartWith(Start, Name, Class, Init, Args) ->
  case Start(Name) of
    {ok, Pid} ->
      ok = addHandler(Class, Pid, Init, Args),
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


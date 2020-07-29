%%---------------------------------------------------------------------------
%% |
%% Module      :  Supervisor
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Supervisor FFI module.
%%
%%---------------------------------------------------------------------------
-module('Supervisor').

-include("../../Foreign.hrl").
-include("./Supervisor/Impl.hrl").

-compile({no_auto_import, [error/1]}).

-export([ startSup/1
        , startSupWith/2
        ]).

-export([ checkChildSpecs/1
        , countChildren/1
        , deleteChild/2
        , getChildSpec/2
        , restartChild/2
        , startChild/2
        , terminateChild/2
        , terminateChildBy/2
        , whichChildren/1
        ]).

-define(MOD, 'Control.Behaviour.Supervisor.Proxy').

-import(?MOD, [translate/1]).

startSup(Init) ->
  ?IO(retPid(?SUP:start_link(?MOD, [Init]))).

startSupWith(Name, Init) ->
  ?IO(retPid(?SUP:start_link_with({local, Name}, ?MOD, [Init]))).

checkChildSpecs(ChildSpecs) ->
  ?IO(resultWith(fun childSpecError/1,
                 ?SUP:check_childspecs([translate(Spec) || Spec <- ChildSpecs]))).

countChildren(SupRef) ->
  ?IO(maps:from_list(?SUP:count_children(ref(SupRef)))).

deleteChild(SupRef, Id) ->
  ?IO(resultWith(fun childError/1, ?SUP:delete_child(ref(SupRef), Id))).

getChildSpec(SupRef, Id) ->
  ?IO(case ?SUP:get_childspec(ref(SupRef), Id) of
        {ok, ChildSpec} ->
          ?Just(specRecord(ChildSpec));
        {error, not_found} -> ?Nothing
      end).

restartChild(SupRef, Id) ->
  ?IO(resultWith(fun childError/1,?SUP:restartChild(ref(SupRef), Id))).

startChild(SupRef, ChildSpec) ->
  ?IO(resultWith(fun childError/1, ?SUP:start_child(ref(SupRef), translate(ChildSpec)))).

terminateChild(SupRef, Id) ->
  ?IO(resultWith(fun childError/1, ?SUP:terminate_child(ref(SupRef), Id))).

terminateChildBy(SupRef, Pid) ->
  ?IO(terminateChild(SupRef, Pid)).

%% TODO: fixme later...
whichChildren(SupRef) ->
  ?IO([Id || {Id, _, _, _} <- ?SUP:which_children(ref(SupRef))]).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

retPid({ok, Pid}) -> Pid;
retPid(ignore) -> erlang:error(ignore);
retPid({error, Reason}) -> erlang:error(Reason).

ref({'SupName', Name}) -> Name;
ref({'SupNameAt', Name, Node}) -> {Name, Node};
ref({'SupGlobal', Name}) -> {global, Name};
ref({'SupPid', Pid}) -> Pid.

resultWith(Fun, Result) ->
  case Result of
    ok -> {'Ok', ok};
    {ok, Result} -> {'Ok', Result};
    {error, Reason} ->
      {'Error', Fun(Reason)}
  end.

specRecord(#{ id := ChildId
            , start := {_M, _F, [StartFun]}
            , restart := Restart
            , shutdown := Shutdown
            , type := Type
            , modules := Modules
            }) ->
  #{ childId => ChildId
   , startFun => StartFun
   , restart => construct(Restart)
   , shutdown => construct(Shutdown)
   , childType => construct(Type)
   , modules => Modules
   }.

construct(permanent) -> {'Permanent'};
construct(transient) -> {'Transient'};
construct(temporary) -> {'Temporary'};
construct(brutal_kill) -> {'BrutalKill'};
construct(infinity) -> {'Infinity'};
construct(Time) when is_integer(Time) -> {'Shutdown', Time};
construct(worker) -> {'Worker'};
construct(supervisor) -> {'Supervisor'}.

childError(already_present) -> 'ChildAlreadyPresent';
childError(already_started) -> 'ChildAlreadyStarted';
childError(running) -> 'ChildIsRunning';
childError(restarting) -> 'ChildIsRestarting';
childError(not_found) -> 'ChildNotFound';
childError(simple_one_for_one) -> 'ChildSimpleOneForOne';
childError(_Reason) -> 'ChildError'.

%% TODO:...
childSpecError(_Reason) -> 'ChildSpecError'.


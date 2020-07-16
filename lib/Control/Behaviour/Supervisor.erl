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

startSup(Init) ->
  ?IO(retPid(?SUP:start_link(?MOD, [Init]))).

startSupWith(Name, Init) ->
  ?IO(retPid(?SUP:start_link_with({local, list_to_atom(Name)}, ?MOD, [Init]))).

%% TODO: destruct childspecs
checkChildSpecs(ChildSpecs) ->
  ?IO(case ?SUP:check_childspecs(ChildSpecs) of
        ok -> ok();
        {error, Reason} ->
          err(childSpecError(Reason))
      end).

countChildren(SupRef) ->
  ?IO(maps:from_list(?SUP:count_children(toErl(SupRef)))).

deleteChild(SupRef, Id) ->
  ?IO(case ?SUP:delete_child(toErl(SupRef), Id) of
        ok -> ok();
        {error, Reason} ->
          err(childError(Reason))
      end).

getChildSpec(SupRef, Id) ->
  ?IO(case ?SUP:get_childspec(toErl(SupRef), Id) of
        {ok, ChildSpec} ->
          just(childSpecRecord(ChildSpec));
        {error, not_found} -> nothing()
      end).

restartChild(SupRef, Id) ->
  ?IO(case ?SUP:restartChild(toErl(SupRef), Id) of
        {ok, Child} ->
          ok(childRecord(Child));
        {error, Reason} ->
          err(childError(Reason))
      end).

startChild(SupRef, ChildSpec) ->
  ?IO(case ?SUP:start_child(toErl(SupRef), ChildSpec) of
        {ok, ChildPid} ->
          ok(ChildPid);
        {error, Reason} ->
          err(childError(Reason))
      end).

terminateChild(SupRef, Id) ->
  ?IO(case ?SUP:terminate_child(toErl(SupRef), Id) of
        ok -> ok();
        {error, Reason} ->
          err(childError(Reason))
      end).

terminateChildBy(SupRef, Pid) ->
  ?IO(terminateChild(SupRef, Pid)).

%% TODO: fixme later...
whichChildren(SupRef) ->
  ?IO([Id || {Id, _, _, _} <- ?SUP:which_children(toErl(SupRef))]).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

-compile({inline, [ok/0, ok/1]}).
ok() -> {'Ok', ok}.
ok(Result) -> {'Ok', Result}.

-compile({inline, [err/1]}).
err(Reason) -> {'Error', Reason}.

-compile({inline, [nothing/0]}).
nothing() -> {'Nothing'}.

-compile({inline, [just/1]}).
just(Result) -> {'Just', Result}.

toErl({'SupName', Name}) -> Name;
toErl({'SupNameAt', Name, Node}) -> {Name, Node};
toErl({'SupGlobal', Name}) -> {global, Name};
toErl({'SupPid', Pid}) -> Pid.

retPid({ok, Pid}) -> Pid;
retPid(ignore) -> erlang:error(ignore);
retPid({error, Reason}) -> erlang:error(Reason).

%% TODO:...
childRecord(Child) -> Child.

%% TODO:...
childSpecRecord(ChildSpec) -> ChildSpec.

childError(already_present) -> 'ChildAlreadyPresent';
childError(already_started) -> 'ChildAlreadyStarted';
childError(running) -> 'ChildIsRunning';
childError(restarting) -> 'ChildIsRestarting';
childError(not_found) -> 'ChildNotFound';
childError(simple_one_for_one) -> 'ChildSimpleOneForOne';
childError(_Reason) -> 'ChildError'.

%% TODO:...
childSpecError(_Reason) -> 'ChildSpecError'.


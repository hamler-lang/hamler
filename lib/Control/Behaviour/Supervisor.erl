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

-compile({no_auto_import, [error/1]}).

-export([ startSupervisor/2
        , checkChildSpecs/1
        , countChildren/1
        , deleteChild/2
        , getChildSpec/2
        , restartChild/2
        , startChild/2
        , terminateChild/2
        , terminateChildByPid/2
        , whichChildren/1
        ]).

-define(MOD, 'Control.Behaviour.Supervisor.Proxy').

%% TODO:
startSupervisor(Class, Args) ->
    {ok, Pid} = supervisor:start_link(?MOD, [Class, Args]),
    Pid.

%% TODO: destruct childspecs
checkChildSpecs(ChildSpecs) ->
    case supervisor:check_childspecs(ChildSpecs) of
        ok -> ok();
        {error, Reason} ->
            err(childSpecError(Reason))
    end.

%% TODO:
countChildren(SupRef) ->
    maps:from_list(supervisor:count_children(supRef(SupRef))).

%% TODO:
deleteChild(SupRef, Id) ->
    case supervisor:delete_child(SupRef, Id) of
        ok -> ok();
        {error, Reason} ->
            err(childError(Reason))
    end.

%% TODO:
getChildSpec(SupRef, Id) ->
    case supervisor:get_childspec(supRef(SupRef), Id) of
        {ok, ChildSpec} ->
            just(childSpecRecord(ChildSpec));
        {error, not_found} -> nothing()
    end.

%% TODO:
restartChild(SupRef, Id) ->
    case supervisor:restartChild(supRef(SupRef), Id) of
        {ok, Child} ->
            ok(childRecord(Child));
        {error, Reason} ->
            err(childError(Reason))
    end.

startChild(SupRef, ChildSpec) ->
    case supervisor:start_child(supRef(SupRef), ChildSpec) of
        {ok, ChildPid} -> ok(ChildPid);
        {error, Reason} ->
            err(childError(Reason))
    end.

terminateChildByPid(SupRef, Pid) ->
    terminateChild(SupRef, Pid).

%% TODO:
terminateChild(SupRef, ChildId) ->
    case supervisor:terminate_child(supRef(SupRef), ChildId) of
        ok -> ok(ok);
        {error, Reason} ->
            err(childError(Reason))
    end.

%% TODO:
whichChildren(SupRef) ->
    [Id || {Id, _, _, _} <- supervisor:which_children(supRef(SupRef))].

-compile({inline, [ok/0, ok/1]}).
ok() -> {'Ok', ok}.
ok(Result) -> {'Ok', Result}.

-compile({inline, [err/1]}).
err(Reason) -> {'Error', Reason}.

-compile({inline, [nothing/0]}).
nothing() -> {'Nothing'}.

-compile({inline, [just/1]}).
just(Result) -> {'Just', Result}.

supRef({'SupName', Name}) -> Name;
supRef({'SupNameOn', Name, Node}) -> {Name, Node};
supRef({'SupGlobal', Name}) -> {global, Name};
supRef({'SupNameVia', Module, Name}) -> {via, Module, Name};
supRef({'SupPid', Pid}) -> Pid.

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


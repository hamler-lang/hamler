%%---------------------------------------------------------------------------
%% |
%% Module      :  Global
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Global FFI module.
%%
%%---------------------------------------------------------------------------
-module('Global').

-include("../../Foreign.hrl").

-export([ delLock/1
        , delLockAt/2
        , notifyAllName/3
        , randomExitName/3
        , randomNotifyName/3
        , registerName/2
        , registerNameWith/3
        , reRegisterName/2
        , reRegisterNameWith/3
        , registeredNames/0
        , send/2
        , setLock/1
        , setLockAt/2
        , setLockRetriesAt/3
        , sync/0
        , trans/2
        , transAt/3
        , transRetriesAt/4
        , unregisterName/1
        , whereisName/1
        ]).

delLock(Id) ->
  ?IO(ok(global:del_lock(Id))).

delLockAt(Id, Nodes) ->
  ?IO(ok(global:del_lock(Id, Nodes))).

notifyAllName(Name, Pid1, Pid2) ->
  ?IO(ok(global:notify_all_name(Name, Pid1, Pid2))).

randomExitName(Name, Pid1, Pid2) ->
  ?IO(ok(global:random_exit_name(Name, Pid1, Pid2))).

randomNotifyName(Name, Pid1, Pid2) ->
  ?IO(ok(global:random_notify_name(Name, Pid1, Pid2))).

registerName(Name, Pid) ->
  ?IO(bool(global:register_name(Name, Pid))).

registerNameWith(Name, Pid, Resolve) ->
  ?IO(bool(global:register_name(Name, Pid, Resolve))).

reRegisterName(Name, Pid) ->
  ?IO(bool(global:re_register_name(Name, Pid))).

reRegisterNameWith(Name, Pid, Resolve) ->
  ?IO(bool(global:re_register_name(Name, Pid, Resolve))).

registeredNames() ->
  ?IO(global:registered_names()).

send(Name, Msg) ->
  ?IO(global:send(Name, Msg)).

setLock(Id) ->
  ?IO(global:set_lock(Id)).

setLockAt(Id, Nodes) ->
   ?IO(global:set_lock(Id, Nodes)).

setLockRetriesAt(Id, Nodes, Retries) ->
   ?IO(global:set_lock(Id, Nodes, Retries)).

sync() -> ?IO(ok(global:sync())).

trans(Id, Fun) ->
  ?IO(maybeAbort(global:trans(Id, Fun))).

transAt(Id, Fun, Nodes) ->
  ?IO(maybeAbort(global:trans(Id, Fun, Nodes))).

transRetriesAt(Id, Fun, Nodes, Retries) ->
  ?IO(maybeAbort(global:trans(Id, Fun, Nodes, Retries))).

unregisterName(Name) ->
  ?IO(ok(global:unregister_name(Name))).

whereisName(Name) ->
  ?IO(case global:whereis_name(Name) of
        undefined -> {'Nohting'};
        Pid -> {'Just', Pid}
      end).

maybeAbort(aborted) ->
  error(aborted);
maybeAbort(Res) -> Res.

ok(_) -> {ok}.

bool(yes) -> true;
bool(no) -> false.

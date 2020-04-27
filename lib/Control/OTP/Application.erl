%%---------------------------------------------------------------------------
%% |
%% Module      :  Application
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Application FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Application').

-export([ ensureAllStarted/1
        , ensureAllRestarted/2
        , start/1
        , restart/2
        , stop/1
        , unload/1
        ]).

ensureAllStarted(Application) ->
    case application:ensure_all_started(atom(Application)) of
        {ok, Started} -> appList(Started);
        {error, Reason} -> error(Reason)
    end.

ensureAllRestarted(Application, Type) ->
    case application:ensure_all_started(atom(Application), restartType(Type)) of
        {ok, Started} -> appList(Started);
        {error, Reason} -> error(Reason)
    end.

start(Application) ->
    case application:start(atom(Application)) of
        ok -> ok;
        {error, Reason} -> error(Reason)
    end.

restart(Application, Type) ->
    case application:start(atom(Application), restartType(Type)) of
        ok -> ok;
        {error, Reason} -> error(Reason)
    end.

stop(Application) ->
    application:stop(atom(Application)).

unload(Application) ->
    application:unload(atom(Application)).

restartType({'Permanent'}) -> permanent;
restartType({'Transient'}) -> transient;
restartType({'Temporary'}) -> temporary.

appList(Apps) -> [atom_to_list(App) || App <- Apps].

atom(S) -> list_to_existing_atom(S).


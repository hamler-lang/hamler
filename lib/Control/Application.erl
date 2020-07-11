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
%% The Application FFI module.
%%
%%---------------------------------------------------------------------------
-module('Application').

-include("../Foreign.hrl").

-export([ ensureAllStarted/1
        , ensureAllRestarted/2
        , ensureStarted/1
        , ensureRestarted/2
        , getApplication/0
        , getApplicationOfPid/1
        , load/1
        , loadedApplications/0
        , start/1
        , restart/2
        , stop/1
        , takeover/2
        , unload/1
        , whichApplications/0
        ]).

ensureAllStarted(Application) ->
  ?IO(return(application:ensure_all_started(Application))).

ensureAllRestarted(Application, Type) ->
  ?IO(return(application:ensure_all_started(Application, restartType(Type)))).

ensureStarted(Application) ->
  ?IO(return(application:ensure_started(Application))).

ensureRestarted(Application, Type) ->
  ?IO(return(application:ensure_started(Application, restartType(Type)))).

getApplication() ->
  ?IO(case application:get_application() of
        undefined -> {'Nothing'};
        {ok, App} -> {'Just', App}
      end).

getApplicationOfPid(Pid) ->
  ?IO(case application:get_application(Pid) of
        undefined -> {'Nothing'};
        {ok, App} -> {'Just', App}
      end).

load(Application) ->
  ?IO(case catch application:load(Application) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok;
        {error, Reason} -> error(Reason);
        {'EXIT', _} -> error('AppNotFound')
      end).

loadedApplications() ->
  ?IO(lists:map(fun appDescr/1, application:loaded_applications())).

start(Application) ->
  ?IO(case catch application:start(Application) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> error(Reason);
        {'EXIT', {"no such file or directory", _}} ->
            error('AppNotFound')
      end).

restart(Application, Type) ->
  ?IO(case catch application:start(Application, restartType(Type)) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> error(Reason);
        {'EXIT', {"no such file or directory", _}} ->
          error('AppNotFound')
    end).

stop(Application) ->
  ?IO(case catch application:stop(Application) of
        ok -> ok;
        {error, {not_started, _}} ->
          error('AppNotStarted');
        {'EXIT', _} ->
          error('AppNotFound')
      end).

takeover(Application, Type) ->
  ?IO(case catch application:takeover(Application, restartType(Type)) of
        ok -> ok;
        {error, Reason} ->
          error(Reason);
        {'EXIT', _} ->
          error('AppNotFound')
    end).

unload(Application) ->
  ?IO(case catch application:unload(Application) of
        ok -> ok;
        {error, {not_loaded, _}} ->
          error('AppNotLoaded');
        {'EXIT', _} ->
          error('AppNotFound')
      end).

whichApplications() ->
  ?IO(lists:map(fun appDescr/1, application:which_applications())).

restartType({'Permanent'}) -> permanent;
restartType({'Transient'}) -> transient;
restartType({'Temporary'}) -> temporary.

%% appStartError(_Reason) -> {'AppStartError'}.

appDescr({App, Descr, Vsn}) ->
  #{name => App, desc => Descr, vsn => Vsn}.

return(ok) -> ok;
return({ok, Result}) -> Result;
return({error, Reason}) -> error(Reason).


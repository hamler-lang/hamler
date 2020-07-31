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

-import(application,
        [ ensure_all_started/1
        , ensure_all_started/2
        , ensure_started/1
        , ensure_started/2
        ]).

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

-import('Maybe', [maybe/1]).

ensureAllStarted(Application) ->
  ?IO(return(ensure_all_started(Application))).

ensureAllRestarted(Application, Type) ->
  ?IO(return(ensure_all_started(Application, translate(Type)))).

ensureStarted(Application) ->
  ?IO(return(ensure_started(Application))).

ensureRestarted(Application, Type) ->
  ?IO(return(ensure_started(Application, translate(Type)))).

getApplication() ->
  ?IO(maybe(application:get_application())).

getApplicationOfPid(Pid) ->
  ?IO(maybe(application:get_application(Pid))).

load(Application) ->
  ?IO(return(application:load(Application))).

loadedApplications() ->
  ?IO(lists:map(fun appDescr/1, application:loaded_applications())).

start(Application) ->
  ?IO(return(application:start(Application))).

restart(Application, Type) ->
  ?IO(return(application:start(Application, translate(Type)))).

stop(Application) ->
  ?IO(return(application:stop(Application))).

takeover(Application, Type) ->
  ?IO(return(application:takeover(Application, translate(Type)))).

unload(Application) ->
  ?IO(return(application:unload(Application))).

whichApplications() ->
  ?IO(lists:map(fun appDescr/1, application:which_applications())).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

translate({'Permanent'}) -> permanent;
translate({'Transient'}) -> transient;
translate({'Temporary'}) -> temporary.

appDescr({App, Descr, Vsn}) ->
  #{name => App, desc => Descr, vsn => Vsn}.

return(ok) -> ok;
return({ok, Result}) -> Result;
return({error, {already_loaded, _}}) -> ok;
return({error, {already_started, _}}) -> ok;
return({error, {not_loaded, _}}) -> error('AppNotLoaded');
return({error, {not_started, _}}) -> error('AppNotStarted');
return({error, {"no such file or directory", Name}}) ->
  error({'AppNotFound', Name});
return({error, Reason}) -> error(Reason).


%%---------------------------------------------------------------------------
%% |
%% Module      :  Proxy
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Supervisor Proxy FFI.
%%
%%---------------------------------------------------------------------------
-module('Proxy').

-behaviour(supervisor).

%% supervisor callbacks
-export([init/1, mfargs/1]).

-define(MOD, 'Control.Behaviour.Supervisor.Proxy').

init([Init]) ->
  {'Supervisor', Restart, ChildSpecs} = Init(),
  SupFlags = {trans(Restart), 10, 100},
  {ok, {SupFlags, trans(ChildSpecs)}}.

trans({'OneForAll'}) ->
  one_for_all;
trans({'OneForOne'}) ->
  one_for_one;
trans({'RestForOne'}) ->
  rest_for_one;
trans({'SimpleOneForOne'}) ->
  simple_one_for_one;

trans({'Permanent'}) -> permanent;
trans({'Transient'}) -> transient;
trans({'Temporary'}) -> temporary;

trans({'BrutalKill'}) -> brutal_kill;
trans({'Infinity'}) -> infinity;
trans({'Shutdown', I}) -> I;

trans({'Worker'}) -> worker;
trans({'Superviso'}) -> supervisor;

trans(ChildSpecs) when is_list(ChildSpecs) ->
  [childSpec(Spec) || Spec <- ChildSpecs].

childSpec(#{ childId := ChildId
           , startFun := StartFun
           , restart := Restart
           , shutdown := Shutdown
           , childType :=  ChildType
           , modules := Modules
           }) ->
  #{ id => ChildId
   , start => {?MOD, mfargs, [StartFun]}
   , restart => trans(Restart)
   , shutdown => trans(Shutdown)
   , type => trans(ChildType)
   , modules => Modules
   }.

mfargs(StartFun) -> StartFun().


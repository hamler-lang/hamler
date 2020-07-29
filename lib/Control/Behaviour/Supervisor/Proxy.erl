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
-export([init/1, apply/1, translate/1]).

-define(MOD, 'Control.Behaviour.Supervisor.Proxy').

init([Init]) ->
  case Init() of
    {'InitOk', SupFlags, ChildSpecs} ->
      {ok, {translate(SupFlags), lists:map(fun translate/1, ChildSpecs)}};
    {'InitIgnore'} -> ignore
  end.

apply(StartFun) -> StartFun().

translate({'OneForAll'}) -> one_for_all;
translate({'OneForOne'}) -> one_for_one;
translate({'RestForOne'}) -> rest_for_one;
translate({'SimpleOneForOne'}) -> simple_one_for_one;

translate({'Permanent'}) -> permanent;
translate({'Transient'}) -> transient;
translate({'Temporary'}) -> temporary;

translate({'BrutalKill'}) -> brutal_kill;
translate({'Infinity'}) -> infinity;
translate({'Shutdown', Time}) -> Time;

translate({'Worker'}) -> worker;
translate({'Supervisor'}) -> supervisor;

translate({Strategy, Intensity, Period}) ->
  {translate(Strategy), Intensity, Period};

translate(#{ childId := ChildId
           , startFun := StartFun
           , restart := Restart
           , shutdown := Shutdown
           , childType :=  ChildType
           , modules := Modules
           }) ->
  #{ id => ChildId
   , start => {?MOD, apply, [StartFun]}
   , restart => translate(Restart)
   , shutdown => translate(Shutdown)
   , type => translate(ChildType)
   , modules => Modules
   }.


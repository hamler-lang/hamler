%%---------------------------------------------------------------------------
%% |
%% Module      :  Client
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The GenServer Client FFI module.
%%
%---------------------------------------------------------------------------
-module('Client').

-export([ abcast/2
        , abcastOn/3
        , callTimeout/3
        , multiCall/2
        , multiCallOn/3
        ]).

abcast(Name, Req) ->
  gen_server:abcast(Name, Req), ok.

abcastOn(Nodes, Name, Req) ->
  gen_server:abcast(Nodes, Name, Req), ok.

callTimeout(ServerRef, Req, Timeout) ->
  gen_server:call(destruct(ServerRef), Req, destruct(Timeout)).

multiCall(Name, Req) ->
  gen_server:multi_call(Name, Req).

multiCallOn(Nodes, Name, Req) ->
  gen_server:multi_call(Nodes, Name, Req).

-compile({inline, [destruct/1]}).
destruct({'ServerPid', Pid}) -> Pid;
destruct({'ServerRef', Name}) -> Name;
destruct({'ServerRefOn', Name, Node}) -> {Name, Node};
destruct({'ServerGlobal', Name}) -> {global, Name};
destruct({'ServerVia', Module, Name}) -> {via, Module, Name};
destruct({'Infinity'}) -> infinity;
destruct({'Timeout', I}) -> I.

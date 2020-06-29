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
%% The GenStatem Client FFI module.
%%
%%---------------------------------------------------------------------------
-module('Client').

-export([ call/2
        , callTimeout/3
        , cast/2
        ]).

call(StatemRef, Req) ->
  gen_statem:call(ref(StatemRef), Req).

callTimeout(StatemRef, Req, Timeout) ->
  gen_statem:call(ref(StatemRef), Req, timeout(Timeout)).

cast(StatemRef, Msg) ->
  gen_statem:cast(ref(StatemRef), Msg).

ref({'StatemPid', Pid}) -> Pid;
ref({'StatemRef', LocalName}) -> LocalName;
ref({'StatemRefOn', LocalName, Node}) -> {LocalName, Node};
ref({'StatemGlobal', GlobalName}) -> GlobalName;
ref({'StatemVia', Module, ViaName}) -> {via, Module, ViaName}.

timeout({'Infinity'}) -> infinity;
timeout({'Timeout', I}) -> I.


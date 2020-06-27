%%---------------------------------------------------------------------------
%% |
%% Module      :  GenStatem
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The GenStatem FFI module.
%%
%%---------------------------------------------------------------------------
-module('GenStatem').

-export([ cast/2
        , call/2
        , startStatem/2
        , shutdown/3
        , stop/1
        ]).

-define(MOD, 'Control.Behaviour.GenStatem.Behaviour').

startStatem(Class, Args) ->
    {ok, Pid} = gen_statem:start_link(?MOD, [Class, Args], []),
    Pid.

cast(StatemRef, Msg) ->
    gen_statem:cast(ref(StatemRef), Msg).

call(StatemRef, Req) ->
    gen_statem:call(ref(StatemRef), Req).

stop(StatemRef) ->
    gen_statem:stop(ref(StatemRef)).

shutdown(StatemRef, Reason, Timeout) ->
    gen_statem:stop(ref(StatemRef), Reason, timeout(Timeout)).

timeout({'Infinity'}) -> infinity;
timeout({'Timeout', I}) -> I.

ref({'StatemPid', Pid}) -> Pid;
ref({'StatemRef', LocalName}) -> LocalName;
ref({'StatemRefOn', LocalName, Node}) -> {LocalName, Node};
ref({'StatemGlobal', GlobalName}) -> GlobalName;
ref({'StatemVia', Module, ViaName}) -> {via, Module, ViaName}.


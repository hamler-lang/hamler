%%---------------------------------------------------------------------------
%% |
%% Module      :  Process
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Process Module.
%%
%%---------------------------------------------------------------------------
-module('Process').

-compile({no_auto_import, [node/1, spawn/2]}).

-export([ node/1
        , mod/1
        , selfNode/0
        , selfPid/0
        , spawn/2
        , send/2
        , 'receive'/0
        , receiveAfter/1
        , 'link'/1
        , 'monitor'/1
        , isProcessAlive/1
        ]).

-spec(node(string()) -> atom()).
node(S) -> list_to_atom(S).

-spec(mod(string()) -> atom()).
mod(S) -> list_to_existing_atom(S).

-spec(selfNode() -> node()).
selfNode() -> node().

-spec(selfPid() -> pid()).
selfPid() -> self().

spawn(Fun, Arg) ->
    erlang:spawn(fun() -> Fun(Arg) end).

-spec(send(pid(), term()) -> term()).
send(Pid, Msg) -> erlang:send(Pid, Msg).

-spec('receive'() -> term()).
'receive'() -> receive X -> X end.

%% TODO: Fixme later:(
-spec(receiveAfter(integer()) -> term()).
receiveAfter(Timeout) ->
    receive X -> X after Timeout -> ok end.

-spec('link'(pid()) -> boolean()).
'link'(Pid) -> erlang:link(Pid).

-spec('monitor'(pid()) -> reference()).
'monitor'(Pid) -> erlang:monitor(process, Pid).

-spec(isProcessAlive(pid()) -> boolean()).
isProcessAlive(Pid) -> erlang:is_process_alive(Pid).


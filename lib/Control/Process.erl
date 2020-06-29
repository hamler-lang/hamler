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
%% The Process FFI module.
%%
%%---------------------------------------------------------------------------
-module('Process').

-compile(no_auto_import).

-export([ node/1
        , selfNode/0
        , selfPid/0
        , spawn/2
        , send/2
        , 'receive'/0
        , receiveAfter/1
        , 'monitor'/1
        , trapExit/1
        , whereis/1
        , kill/1
        ]).

-spec(node(string()) -> atom()).
node(S) -> erlang:list_to_atom(S).

-spec(selfNode() -> node()).
selfNode() -> erlang:node().

-spec(selfPid() -> pid()).
selfPid() -> erlang:self().

spawn(Fun, Arg) -> erlang:spawn(fun() -> Fun(Arg) end).

-spec(send(pid(), term()) -> term()).
send(Pid, Msg) -> erlang:send(Pid, Msg).

-spec('receive'() -> term()).
'receive'() -> receive X -> X end.

%% TODO: Fixme later:(
-spec(receiveAfter(integer()) -> term()).
receiveAfter(Timeout) ->
    receive X -> X after Timeout -> ok end.

-spec('monitor'(pid()) -> reference()).
'monitor'(Pid) -> erlang:monitor(process, Pid).

-spec(trapExit(boolean()) -> boolean()).
trapExit(Flag) -> erlang:process_flag(trap_exit, Flag).

whereis(Name) ->
  case erlang:whereis(Name) of
    undefined -> {'Nothing'};
    Pid -> {'Just', Pid}
  end.

kill(Pid) -> erlang:exit(Pid, kill).


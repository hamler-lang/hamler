%%---------------------------------------------------------------------------
%% |
%% Module      :  GenServer
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The GenServer FFI module.
%%
%%---------------------------------------------------------------------------
-module('GenServer').

-export([ start/3
        , startLink/3
        , startMonitor/3
        , abcast/2
        , abcastOn/3
        , call/2
        , callTimeout/3
        , multiCall/2
        , multiCallOn/3
        , cast/2
        , reply/3
        , noReply/1
        , stop/1
        ]).

-export([init_it/1]).

start(Class, Init, Args) ->
  proc_lib:spawn_link(
    fun() ->
        init_it([self(), Class, Init, Args])
    end).

startLink(Class, Init, Args) ->
  start(Class, Init, Args).

startMonitor(Class, Init, Args) ->
  proc_lib:spawn_monitor(
    fun() ->
        init_it([self(), Class, Init, Args])
    end).

stop(ServerRef) ->
  gen_server:stop(destruct(ServerRef)).

abcast(Name, Req) ->
  gen_server:abcast(Name, Req), ok.

abcastOn(Nodes, Name, Req) ->
  gen_server:abcast(Nodes, Name, Req), ok.

call(ServerRef, Req) ->
  gen_server:call(destruct(ServerRef), Req).

callTimeout(ServerRef, Req, Timeout) ->
  gen_server:call(destruct(ServerRef), Req, destruct(Timeout)).

cast(ServerRef, Req) ->
  gen_server:cast(destruct(ServerRef), Req).

multiCall(Name, Req) ->
  gen_server:multi_call(Name, Req).

multiCallOn(Nodes, Name, Req) ->
  gen_server:multi_call(Nodes, Name, Req).

reply(From, Rep, State) ->
  ok = gen_server:reply(From, Rep), State.

noReply(State) -> State.

init_it([Parent, Class, Init, Args]) ->
  enter_loop(Class, Parent, Init(Args)).

enter_loop(#{handleCall := HandleCall,
             handleCast := HandleCast
            } = Class, Parent, State) ->
  receive
    {'$gen_call', From, Req} ->
      NState = uncurry(HandleCall, [Req, From, State]),
      enter_loop(Class, Parent, NState);
    {'$gen_cast', Msg} ->
      NState = uncurry(HandleCast, [Msg, State]),
      enter_loop(Class, Parent, NState);
    Msg ->
      io:format("Unexpected Msg: ~p~n", [Msg]),
      enter_loop(Class, Parent, State)
  end.

-compile({inline, [destruct/1]}).
destruct({'ServerPid', Pid}) -> Pid;
destruct({'ServerRef', Name}) -> Name;
destruct({'ServerRefOn', Name, Node}) -> {Name, Node};
destruct({'ServerGlobal', Name}) -> {global, Name};
destruct({'ServerVia', Module, Name}) -> {via, Module, Name};

destruct({'Infinity'}) -> infinity;
destruct({'Timeout', I}) -> I.

uncurry(Fun, [H|T]) -> uncurry(Fun(H), T);
uncurry(Ret, []) -> Ret.

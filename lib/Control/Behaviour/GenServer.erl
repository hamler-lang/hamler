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

-export([ startServer/2
        , call/2
        , cast/2
        , reply/3,
        , noReply/2
        ]).

-export([init_it/1]).

startServer(Class, Init, Args) ->
  InitIt = fun() ->
               init_it([self(), Class, Init, Args])
           end,
  Pid = proc_lib:spawn_link(InitIt).

call(Pid, Req) -> gen_server:call(Pid, Req).

cast(Pid, Msg) -> gen_server:cast(Pid, Msg).

reply(From, Rep, State) ->
  ok = gen_server:reply(From, Rep), State.

noReply(State) -> State.

init_it([Parent, Class, Init, Args]) ->
  enter_loop(Class, Parent, Init(Args)).

enter_loop(#{handleCall := handleCall,
             handleCast := handleCast
            } = Class, Parent, State) ->
  receive
    {'$gen_call', From, Req} ->
      NState = handleCall(Req, From, State),
      enter_loop(Class, Parent, NState);
    {'$gen_cast', Msg} ->
      NState = handleCast(Msg, State),
      enter_loop(Class, Parent, NState);
    Msg ->
      io:format("Unexpected Msg: ~p~n", [Msg]),
      enter_loop(Class, Parent, State)
  end.


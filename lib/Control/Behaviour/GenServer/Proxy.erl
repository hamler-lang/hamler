%%---------------------------------------------------------------------------
%% |
%% Module      :  Proxy
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The GenServer Proxy FFI module.
%%
%%---------------------------------------------------------------------------
-module('Proxy').

-behaviour(gen_server).

-compile({no_auto_import, [apply/2]}).

-include("../../../Foreign.hrl").

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_continue/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-import('Curry', [apply/2]).

-record(proxy, {class, state}).

init([Class, Init]) ->
  case Init() of
    {'InitOk', State, ?Nothing} ->
      {ok, initOk(Class, State)};
    {'InitOk', State, ?Just(Action)} ->
      {ok, initOk(Class, State), translate(Action)};
    {'InitIgnore'} ->
      ignore;
    {'InitStop', Reason} ->
      {stop, shutdown(Reason)}
  end.

initOk(Class, State) ->
  #proxy{class = Class, state = State}.

handle_call(Request, From, Proxy = #proxy{class = #{handleCall := HandleCall}, state = State}) ->
  case ?RunIO(apply(HandleCall, [Request, From, State])) of
    {'Reply', Rep, NState, ?Nothing} ->
      {reply, Rep, Proxy#proxy{state = NState}};
    {'Reply', Rep, NState, ?Just(Action)} ->
      {reply, Rep, Proxy#proxy{state = NState}, translate(Action)};
    {'NoReply', NState, ?Nothing} ->
      {noreply, Proxy#proxy{state = NState}};
    {'NoReply', NState, ?Just(Action)} ->
      {noreply, Proxy#proxy{state = NState}, translate(Action)};
    {'Shutdown', Reason, NState} ->
      {stop, shutdown(Reason), Proxy#proxy{state = NState}}
  end.

handle_continue(Continue, Proxy) -> handle_cast(Continue, Proxy).

handle_cast(Msg, Proxy = #proxy{class = #{handleCast := HandleCast}, state = State}) ->
  case ?RunIO(apply(HandleCast, [Msg, State])) of
    {'NoReply', NState, ?Nothing} ->
      {noreply, Proxy#proxy{state = NState}};
    {'NoReply', NState, ?Just(Action)} ->
      {noreply, Proxy#proxy{state = NState}, translate(Action)};
    {'Shutdown', Reason, NState} ->
      {stop, shutdown(Reason), Proxy#proxy{state = NState}}
  end.

handle_info(Info, Proxy) ->
  error_logger:error_msg("Unexpected Info: ~p", [Info]),
  {noreply, Proxy}.

terminate(_Reason, _Proxy) -> ok.

code_change(_OldVsn, Proxy, _Extra) ->
  {ok, Proxy}.

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

translate({'Continue', Req}) -> {continue, Req};
translate({'Hibernate'}) -> hibernate;
translate({'Timeout', Time}) -> Time.

shutdown({'ExitNormal'}) -> normal;
shutdown({'ExitShutdown'}) -> shutdown;
shutdown({'ExitReason', Reason}) -> {shutdown, Reason}.

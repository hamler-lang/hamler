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
%% The GenStatem Behaviour FFI.
%%
%%---------------------------------------------------------------------------
-module('Proxy').

-behaviour(gen_statem).

-export([ init/1
        , callback_mode/0
        , handle_event/4
        , terminate/3
        , code_change/4
        ]).

-record(proxy, {handleEvent, state}).

init([#{handleEvent := handleEvent}, Init]) ->
  {ok, idle, #proxy{handleEvent = handleEvent, state = Init()}}.

callback_mode() -> handle_event_function.

handle_event(EventType, Event, State, Data) ->
  %% Ignore all other events
  io:format("EventType: ~p, Event: ~p~n", [EventType, Event]),
  {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.


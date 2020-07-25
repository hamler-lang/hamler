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

-include("../../../Foreign.hrl").

-behaviour(gen_statem).

-export([ init/1
        , callback_mode/0
        , handle_event/4
        , terminate/3
        , code_change/4
        ]).

-record(proxy, {handleEvent, data}).

init([Class, Init]) ->
  case Init() of
    {'InitOk', State, Data} ->
      {ok, State, init_ok(Class, Data)};
    {'InitOkHib', State, Data} ->
      {ok, State, init_ok(Class, Data), hibernate};
    {'InitIgnore'} ->
      ignore;
    {'InitStop', Reason} ->
      {stop, Reason}
  end.

init_ok(#{handleEvent := HandleEvent}, Data) ->
  #proxy{handleEvent = HandleEvent, data = Data}.

callback_mode() -> handle_event_function.

handle_event(EventType, Event, State,
             Proxy = #proxy{handleEvent = HandleEvent, data = Data}) ->
  io:format("EventType: ~p, Event: ~p~n", [EventType, Event]),
  Args = [wrap(EventType), Event, State, Data],
  case ?RunIO('Curry':apply(HandleEvent, Args)) of
    {'Keep', NData} ->
      {keep_state, Proxy#proxy{data = NData}};
    {'Next', NState, NData} ->
      {next_state, NState, Proxy#proxy{data = NData}};
    {'Repeat', NData} ->
      {repeat_state, Proxy#proxy{data = NData}}
  end.

terminate(_Reason, _State, _Data) ->
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

wrap({call, From}) -> {'Call', From};
wrap(cast) -> {'Cast'}.


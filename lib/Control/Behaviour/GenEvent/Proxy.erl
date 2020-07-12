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
%% The GenEvent Behaviour FFI module.
%%
%%---------------------------------------------------------------------------
-module('Proxy').

-behaviour(gen_event).

-export([ init/1
        , handle_call/2
        , handle_event/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-import('Curry', [uncurry/2, uncurryIO/2]).

-record(proxy, {handleEvent, state}).

init([#{handleEvent := HandleEvent}, Init]) ->
  case Init() of
    {'InitOk', State} ->
      {ok, #proxy{handleEvent = HandleEvent, state = State}};
    {'InitOkHib', State} ->
      {ok, #proxy{handleEvent = HandleEvent, state = State}, hibernate};
    {'InitError', Reason} ->
      {error, Reason}
  end.

handle_call(_Request, Proxy) ->
  {ok, ignored, Proxy}.

handle_event(Event, Proxy = #proxy{handleEvent = HandleEvent, state = State}) ->
  NState = uncurryIO(HandleEvent, [Event, State]),
  {ok, Proxy#proxy{state = NState}}.

handle_info(Info, Proxy) ->
  error_logger:error_msg("Unexpected Info: ~p", [Info]),
  {ok, Proxy}.

terminate(_Arg, _Proxy) ->
  ok.

code_change(_OldVsn, Proxy, _Extra) ->
  {ok, Proxy}.


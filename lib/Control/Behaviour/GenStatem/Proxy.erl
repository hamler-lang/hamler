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

-record(proxy, {class, data}).

init([Class, Init]) ->
    case Init() of
        {'InitOk', State, Data, Actions} ->
            Proxy = #proxy{class = Class, data = Data},
            {ok, State, Proxy, transAction(Actions)};
        {'InitStop', Reason} ->
            {stop, Reason};
        {'InitIgnore'} -> ignore
    end.

callback_mode() -> handle_event_function.

handle_event(Type, Content, State,
             Proxy = #proxy{class = Class, data = Data}) ->
    #{handleEvent := HandleEvent} = Class,
    Args = [wrap(Type), Content, State, Data],
    case ?RunIO('Curry':apply(HandleEvent, Args)) of
        {'Keep', NData, Actions} ->
            {keep_state, Proxy#proxy{data = NData}, transAction(Actions)};
        {'Next', NState, NData, Actions} ->
            {next_state, NState, Proxy#proxy{data = NData}, transAction(Actions)};
        {'Repeat', NData, Actions} ->
            {repeat_state, Proxy#proxy{data = NData}, transAction(Actions)};
        {'Shutdown', Reason, NData} ->
            {stop, shutdown(Reason), Proxy#proxy{data = NData}}
    end.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

transAction([]) -> [];
transAction(Actions) when is_list(Actions) ->
    lists:map(fun transAction/1, Actions);
transAction({'Postpone'}) -> postpone;
transAction({'NextEvent', Type, Content}) ->
    {next_event, unwrap(Type), Content};
transAction({'Hibernate'}) -> hibernate;
transAction({'TimeoutEvent', Time, Event}) ->
    {timeout, Time, Event};
transAction({'TimeoutCancel'}) ->
    {timeout, cancel};
transAction({'TimeoutUpdate', Event}) ->
    {timeout, update, Event};
transAction({'StateTimeout', Time, Event}) ->
    {state_timeout, Time, Event};
transAction({'StateTimeoutCancel'}) ->
    {state_timeout, cancel};
transAction({'StateTimeoutUpdate', Event}) ->
    {state_timeout, update, Event}.

wrap({call, From}) -> {'Call', From};
wrap(cast) -> {'Cast'};
wrap(info) -> {'Info'};
wrap(timeout) -> {'Timeout'};
wrap(state_timeout) -> {'Timeout'};
wrap(internal) -> {'Internal'}.

unwrap({'Call', From}) -> {call, From};
unwrap({'Cast'}) -> cast;
unwrap({'Info'}) -> info;
unwrap({'Timeout'}) -> timeout;
unwrap({'Internal'}) -> internal.

shutdown({'ExitNormal'}) -> normal;
shutdown({'ExitShutdown'}) -> shutdown;
shutdown({'ExitReason', Reason}) -> {shutdown, Reason}.


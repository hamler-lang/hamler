%%---------------------------------------------------------------------------
%% |
%% Module      :  Behaviour
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The GenEvent Behaviour FFI.
%%
%%---------------------------------------------------------------------------
-module('Behaviour').

-behaviour(gen_event).

-export([ init/1
        , handle_call/2
        , handle_event/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

init([Class = #{init := InitFun}, Args]) ->
    io:format("~p~n", [Args]),
    {ok, #{class => Class, st => InitFun(Args)}}.

handle_call(Request, State) ->
    io:format("Call: ~p~n", [Request]),
    Reply = ok,
    {ok, Reply, State}.

handle_event(Event, State) ->
    io:format("Event: ~p~n", [Event]),
    {ok, State}.

handle_info(Info, State) ->
    io:format("Info: ~p~n", [Info]),
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


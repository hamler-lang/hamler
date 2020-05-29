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
%% The GenServer Behaviour FFI.
%%
%%---------------------------------------------------------------------------
-module('Behaviour').

-behaviour(gen_server).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

init([Class = #{init := InitFun}, Args]) ->
    io:format("~p~n", [Args]),
    {ok, #{class => Class, st => InitFun(Args)}}.

handle_call(Request, _From, State) ->
    io:format("Call: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("Cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


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
%% The Supervisor Behaviour Proxy FFI.
%%
%%---------------------------------------------------------------------------
-module('Proxy').

-behaviour(supervisor).

%% supervisor callbacks
-export([init/1]).

init([Class = #{init := InitFun}, Args]) ->
    io:format("~p~n", [Args]),
    {SupFlags, [ChildSpec]} = InitFun(Args),
    {ok, {SupFlags, [ChildSpec]}}.

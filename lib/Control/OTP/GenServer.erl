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
        ]).

-define(MOD, 'Control.OTP.GenServer.Behaviour').

startServer(Class, Args) ->
    {ok, Pid} = gen_server:start_link(?MOD, [Class, Args], []),
    Pid.

call(Pid, Req) -> gen_server:call(Pid, Req).

cast(Pid, Msg) -> gen_server:cast(Pid, Msg).


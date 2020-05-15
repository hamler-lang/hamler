%%---------------------------------------------------------------------------
%% |
%% Module      :  Env
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Erlang Env Module.
%%
%%---------------------------------------------------------------------------
-module('Env').

-export([ getAllEnv/0
        , getEnv/1
        , lookupEnv/1
        , setEnv/2
        , unsetEnv/1
        ]).

getAllEnv() ->
    [list_to_tuple(string:split(Env, "=")) || Env <- os:getenv()].

getEnv(Name) ->
    case os:getenv(Name) of
        false -> error("EnvDoesNotExistError");
        Value -> Value
    end.

lookupEnv(Name) ->
    case os:getenv(Name) of
        false -> {'Nothing'};
        Value -> {'Just', Value}
    end.

setEnv(Name, Value) -> os:putenv(Name, Value).

unsetEnv(Name) -> os:unsetenv(Name).


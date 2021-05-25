%%---------------------------------------------------------------------------
%% |
%% Module      :  Env
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Erlang Env module.
%%
%%---------------------------------------------------------------------------
-module('Env').

-include("../Foreign.hrl").

-export([ getAllEnv/0
        , getEnv/1
        , lookupEnv/1
        , setEnv/2
        , unsetEnv/1
        ]).

%% getAllEnv :: IO (List (String, String))
getAllEnv() ->
  ?IO([list_to_tuple(string:split(Env, "=")) || Env <- os:getenv()]).

getEnv(Name) ->
  ?IO(case os:getenv(Name) of
        false -> error('EnvDoesNotExist');
        Value -> Value
      end).

lookupEnv(Name) ->
  ?IO(case os:getenv(Name) of
        false -> {'Nothing'};
        Value -> {'Just', Value}
      end).

setEnv(Name, Value) ->
  ?IO(os:putenv(Name, Value)).

unsetEnv(Name) ->
  ?IO(os:unsetenv(Name)).

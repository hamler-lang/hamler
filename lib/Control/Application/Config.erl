%%---------------------------------------------------------------------------
%% |
%% Module      :  Config
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Application Config FFI.
%%
%%---------------------------------------------------------------------------
-module('Config').

-export([ getAtom/2
        , getBool/2
        , getInt/2
        , getFloat/2
        , getString/2
        , getList/2
        , getMap/2
        , getRecord/2
        ]).

getAtom(Key, Config) ->
  getWith(Key, Config, fun is_atom/1).

getBool(Key, Config) ->
  getWith(Key, Config, fun is_boolean/1).

getInt(Key, Config) ->
  getWith(Key, Config, fun is_integer/1).

getFloat(Key, Config) ->
  getWith(Key, Config, fun is_float/1).

getString(Key, Config) ->
  getWith(Key, Config, fun is_list/1).

getList(Key, Config) ->
  getWith(Key, Config, fun is_list/1).

getMap(Key, Config) ->
  getWith(Key, Config, fun is_map/1).

getRecord(Key, Config) ->
  toRecord(getWith(Key, Config, fun isRecord/1)).

getWith(Key, Config, Assert) ->
  case proplists:get_value(Key, Config) of
    undefined -> error('ConfigNotFound');
    Val -> case Assert(Val) of
             true -> Val;
             false -> error('ConfigBadType')
           end
  end.

isRecord(R) -> is_map(R) orelse is_list(R).

toRecord(L) when is_list(L) -> maps:from(L);
toRecord(M) when is_map(M) -> M.

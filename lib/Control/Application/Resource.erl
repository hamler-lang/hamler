%%---------------------------------------------------------------------------
%% |
%% Module      :  Resource
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Application Resource FFI module.
%%
%%---------------------------------------------------------------------------
-module('Resource').

-include("../../Foreign.hrl").

-export([getAppSpec/1]).

-import(proplists, [get_value/3]).

getAppSpec(App) ->
  ?IO(case application:get_all_key(App) of
        {ok, Keys} -> specRec(App, Keys);
        undefined -> error('AppNotFound')
      end).

specRec(App, Keys) ->
  maps:put(name, App, maps:from_list(Keys)).

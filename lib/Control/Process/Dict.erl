%%---------------------------------------------------------------------------
%% |
%% Module      :  Dict
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Dict FFI module.
%%
%%---------------------------------------------------------------------------
-module('Dict').

-include("../../Foreign.hrl").

-compile(no_auto_import).

-export([ erase/1
        , eraseAll/0
        ]).

erase(Key) ->
  ?IO(maybe(erlang:erase(Key))).

eraseAll() -> ?IO(erlang:erase()).

maybe(undefined) -> {'Nothing'};
maybe(Val) -> {'Just', Val}.

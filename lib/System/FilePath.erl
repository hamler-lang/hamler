%%---------------------------------------------------------------------------
%% |
%% Module      :  FilePath
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The FilePath FFI module.
%%
%%---------------------------------------------------------------------------
-module('FilePath').

-include("../Foreign.hrl").

-export([isAbs/1, isRel/1]).

isAbs(Path) ->
  filename:pathtype(Path) == absolute.

isRel(Path) ->
  filename:pathtype(Path) == relative.

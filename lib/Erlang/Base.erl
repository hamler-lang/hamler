%%---------------------------------------------------------------------------
%% |
%% Module      :  Base
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Erlang Base Module.
%%
%%---------------------------------------------------------------------------
-module('Base').

-export([makeRef/0]).

-spec(makeRef() -> reference()).
makeRef() -> erlang:make_ref().


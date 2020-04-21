%%---------------------------------------------------------------------------
%% |
%% Module      :  Ref
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Ref FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Ref').

-export([makeRef/0]).

-spec(makeRef() -> reference()).
makeRef() -> erlang:make_ref().


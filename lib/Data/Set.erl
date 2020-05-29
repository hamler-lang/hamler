%%---------------------------------------------------------------------------
%% |
%% Module      :  Set
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Set FFI module.
%%
%%---------------------------------------------------------------------------
-module('Set').

-export([ singleton/1 ]).

-spec(singleton(term()) -> sets:set()).
singleton(A) -> sets:add_element(A, sets:new()).

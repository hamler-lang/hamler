%%---------------------------------------------------------------------------
%% |
%% Module      :  Foldable
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Foldable FFI module.
%%
%%---------------------------------------------------------------------------
-module('Foldable').

-export([ foldlListImpl/3
        , foldrListImpl/3
        ]).

foldlListImpl(Fun, Acc, List) ->
  F = fun(A, B) -> (Fun(A))(B) end,
  lists:foldl(F, Acc, List).

foldrListImpl(Fun, Acc, List) ->
  F = fun(A, B) -> (Fun(A))(B) end,
  lists:foldr(F, Acc, List).

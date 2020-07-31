%%---------------------------------------------------------------------------
%% |
%% Module      :  Ord
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Ord FFI module.
%%
%%---------------------------------------------------------------------------
-module('Ord').

-export([ cmpAtomImpl/3
        , cmpBoolImpl/3
        , cmpBinImpl/3
        , cmpCharImpl/3
        , cmpIntImpl/3
        , cmpFloatImpl/3
        , cmpStringImpl/3
        ]).

cmpAtomImpl(LT, EQ, GT) ->
  fun(A1) -> fun(A2) -> cmp(LT, EQ, GT, A1, A2) end end.

cmpBoolImpl(LT, EQ, GT) ->
  fun(B1) -> fun(B2) -> cmp(LT, EQ, GT, B1, B2) end end.

cmpBinImpl(LT, EQ, GT) ->
  fun(B1) -> fun(B2) -> cmp(LT, EQ, GT, B1, B2) end end.

cmpCharImpl(LT, EQ, GT) ->
  fun (C1) -> fun(C2) -> cmp(LT, EQ, GT, C1, C2) end end.

cmpIntImpl(LT, EQ, GT) ->
  fun(I1) -> fun(I2) -> cmp(LT, EQ, GT, I1, I2) end end.

cmpFloatImpl(LT, EQ, GT) ->
  fun(F1) -> fun(F2) -> cmp(LT, EQ, GT, F1, F2) end end.

cmpStringImpl(LT, EQ, GT) ->
  fun(S1) -> fun(S2) -> cmp(LT, EQ, GT, S1, S2) end end.

-compile({inline, [cmp/5]}).
cmp(LT, EQ, GT, X, Y) ->
  if X > Y -> GT;
     X < Y -> LT;
     true  -> EQ
  end.

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
%% The Ord Module.
%%
%%---------------------------------------------------------------------------
-module('Ord').

-export([ compBool/3
        , compChar/3
        , compInt/3
        , compFloat/3
        , compNum/3
        , compString/3
        ]).

compBool(LT, GT, EQ) ->
    fun(B1, B2) -> comp(LT, GT, EQ, B1, B2) end.

compChar(LT, GT, EQ) ->
    fun (C1, C2) -> comp(LT, GT, EQ, C1, C2) end.

compInt(LT, GT, EQ) ->
    fun(I1, I2) -> comp(LT, GT, EQ, I1, I2) end.

compFloat(LT, GT, EQ) ->
    fun(F1, F2) -> comp(LT, GT, EQ, F1, F2) end.

compNum(LT, GT, EQ) ->
    fun(N1, N2) -> comp(LT, GT, EQ, N1, N2) end.

compString(LT, GT, EQ) ->
    fun(S1, S2) -> comp(LT, GT, EQ, S1, S2) end.

-compile({inline, [comp/5]}).
comp(LT, GT, EQ, X, Y) ->
    if X > Y -> GT;
       X < Y -> LT;
       true  -> EQ
    end.

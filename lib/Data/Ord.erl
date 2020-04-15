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

-export([ compBool/5
        , compChar/5
        , compInt/5
        , compFloat/5
        , compNum/5
        , compString/5
        ]).

compBool(LT, GT, EQ, B1, B2) ->
    comp(LT, GT, EQ, B1, B2).

compChar(LT, GT, EQ, C1, C2) ->
    comp(LT, GT, EQ, C1, C2).

compInt(LT, GT, EQ, I1, I2) ->
    comp(LT, GT, EQ, I1, I2).

compFloat(LT, GT, EQ, F1, F2) ->
    comp(LT, GT, EQ, F1, F2).

compNum(LT, GT, EQ, N1, N2) ->
    comp(LT, GT, EQ, N1, N2).

compString(LT, GT, EQ, S1, S2) ->
    comp(LT, GT, EQ, S1, S2).

-compile({inline, [comp/5]}).
comp(LT, GT, EQ, X, Y) ->
    if X > Y -> GT;
       X < Y -> LT;
       true  -> EQ
    end.

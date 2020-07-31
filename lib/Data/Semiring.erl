%%---------------------------------------------------------------------------
%% |
%% Module      :  Semiring
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Semiring Module.
%%
%%---------------------------------------------------------------------------
-module('Semiring').

-export([ addInt/2
        , mulInt/2
        , addFloat/2
        , mulFloat/2
        ]).

%% addInt :: Int -> Int -> Int
-spec(addInt(integer(), integer()) -> integer()).
addInt(I1, I2) -> I1 + I2.

%% mulInt :: Int -> Int -> Int
-spec(mulInt(integer(), integer()) -> integer()).
mulInt(I1, I2) -> I1 * I2.

%% addFloat :: Float -> Float -> Float
-spec(addFloat(float(), float()) -> float()).
addFloat(F1, F2) -> F1 + F2.

%% mulFloat :: Float -> Float -> Float
-spec(mulFloat(float(), float()) -> float()).
mulFloat(F1, F2) -> F1 * F2.

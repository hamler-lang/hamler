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

-export([ intAdd/2
        , intMul/2
        , floatAdd/2
        , floatMul/2
        , numAdd/2
        , numMul/2
        ]).

%% intAdd :: Int -> Int -> Int
-spec(intAdd(integer(), integer()) -> integer()).
intAdd(I1, I2) -> I1 + I2.

%% intMul :: Int -> Int -> Int
-spec(intMul(integer(), integer()) -> integer()).
intMul(I1, I2) -> I1 * I2.

%% floatAdd :: Float -> Float -> Float
-spec(floatAdd(float(), float()) -> float()).
floatAdd(F1, F2) -> F1 + F2.

%% floatAdd :: Float -> Float -> Float
-spec(floatMul(number(), number()) -> number()).
floatMul(F1, F2) -> F1 * F2.

%% numAdd :: Number -> Number -> Number
-spec(numAdd(number(), number()) -> number()).
numAdd(N1, N2) -> N1 + N2.

%% numMul :: Number -> Number -> Number
-spec(numMul(number(), number()) -> number()).
numMul(N1, N2) -> N1 * N2.


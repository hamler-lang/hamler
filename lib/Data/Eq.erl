%%---------------------------------------------------------------------------
%% |
%% Module      :  Eq
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Eq Module.
%%
%%---------------------------------------------------------------------------
-module('Eq').

-export([ boolEq/2
        , charEq/2
        , intEq/2
        , floatEq/2
        , numEq/2
        ]).

-spec(boolEq(boolean(), boolean()) -> boolean()).
boolEq(B1, B2) -> B1 == B2.

-spec(charEq(char(), char()) -> boolean()).
charEq(C1, C2) -> C1 == C2.

-spec(intEq(integer(), integer()) -> boolean()).
intEq(I1, I2) -> I1 == I2.

-spec(floatEq(float(), float()) -> boolean()).
floatEq(F1, F2) -> F1 == F2.

-spec(numEq(number(), number()) -> boolean()).
numEq(N1, N2) -> N1 == N2.

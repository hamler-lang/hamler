%%---------------------------------------------------------------------------
%% |
%% Module      :  Ring
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Ring Module.
%%
%%---------------------------------------------------------------------------
-module('Ring').

-export([ charSub/2
        , intSub/2
        , floatSub/2
        , numSub/2
        ]).

-spec(charSub(char(), char()) -> char()).
charSub(C1, C2) -> C1 - C2.

-spec(intSub(integer(), integer()) -> integer()).
intSub(I1, I2) -> I1 - I2.

-spec(floatSub(float(), float()) -> float()).
floatSub(F1, F2) -> F1 - F2.

-spec(numSub(number(), number()) -> number()).
numSub(N1, N2) -> N1 - N2.


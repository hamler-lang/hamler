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

-export([ boolLessThan/2
        , boolGreaterThan/2
        , boolLessThanOrEq/2
        , boolGreaterThanOrEq/2
        ]).

-export([ charLessThan/2
        , charGreaterThan/2
        , charLessThanOrEq/2
        , charGreaterThanOrEq/2
        ]).

-export([ intLessThan/2
        , intGreaterThan/2
        , intLessThanOrEq/2
        , intGreaterThanOrEq/2
        ]).

-export([ floatLessThan/2
        , floatGreaterThan/2
        , floatLessThanOrEq/2
        , floatGreaterThanOrEq/2
        ]).

-export([ numLessThan/2
        , numGreaterThan/2
        , numLessThanOrEq/2
        , numGreaterThanOrEq/2
        ]).


%%---------------------------------------------------------------------------
%% Bool Ord
%%---------------------------------------------------------------------------

-spec(boolLessThan(boolean(), boolean()) -> boolean()).
boolLessThan(B1, B2) -> lessThan(B1, B2).

-spec(boolGreaterThan(boolean(), boolean()) -> boolean()).
boolGreaterThan(B1, B2) -> greaterThan(B1, B2).

-spec(boolLessThanOrEq(boolean(), boolean()) -> boolean()).
boolLessThanOrEq(B1, B2) -> lessThanOrEq(B1, B2).

-spec(boolGreaterThanOrEq(boolean(), boolean()) -> boolean()).
boolGreaterThanOrEq(B1, B2) -> greaterThanOrEq(B1, B2).

%%---------------------------------------------------------------------------
%% Char Ord
%%---------------------------------------------------------------------------

-spec(charLessThan(char(), char()) -> boolean()).
charLessThan(C1, C2) -> lessThan(C1, C2).

-spec(charGreaterThan(char(), char()) -> boolean()).
charGreaterThan(C1, C2) -> greaterThan(C1, C2).

-spec(charLessThanOrEq(char(), char()) -> boolean()).
charLessThanOrEq(C1, C2) -> lessThanOrEq(C1, C2).

-spec(charGreaterThanOrEq(char(), char()) -> boolean()).
charGreaterThanOrEq(C1, C2) -> greaterThanOrEq(C1, C2).

%%---------------------------------------------------------------------------
%% Int Ord
%%---------------------------------------------------------------------------

-spec(intLessThan(integer(), integer()) -> boolean()).
intLessThan(I1, I2) -> lessThan(I1, I2).

-spec(intGreaterThan(integer(), integer()) -> boolean()).
intGreaterThan(I1, I2) -> greaterThan(I1, I2).

-spec(intLessThanOrEq(integer(), integer()) -> boolean()).
intLessThanOrEq(I1, I2) -> lessThanOrEq(I1, I2).

-spec(intGreaterThanOrEq(integer(), integer()) -> boolean()).
intGreaterThanOrEq(I1, I2) -> greaterThanOrEq(I1, I2).

%%---------------------------------------------------------------------------
%% Float Ord
%%---------------------------------------------------------------------------

-spec(floatLessThan(float(), float()) -> boolean()).
floatLessThan(F1, F2) -> lessThan(F1, F2).

-spec(floatGreaterThan(float(), float()) -> boolean()).
floatGreaterThan(F1, F2) -> greaterThan(F1, F2).

-spec(floatLessThanOrEq(float(), float()) -> boolean()).
floatLessThanOrEq(F1, F2) -> lessThanOrEq(F1, F2).

-spec(floatGreaterThanOrEq(float(), float()) -> boolean()).
floatGreaterThanOrEq(F1, F2) -> greaterThanOrEq(F1, F2).

%%---------------------------------------------------------------------------
%% Num Ord
%%---------------------------------------------------------------------------

-spec(numLessThan(number(), number()) -> boolean()).
numLessThan(N1, N2) -> lessThan(N1, N2).

-spec(numGreaterThan(number(), number()) -> boolean()).
numGreaterThan(N1, N2) -> greaterThan(N1, N2).

-spec(numLessThanOrEq(number(), number()) -> boolean()).
numLessThanOrEq(N1, N2) -> lessThanOrEq(N1, N2).

-spec(numGreaterThanOrEq(number(), number()) -> boolean()).
numGreaterThanOrEq(N1, N2) -> greaterThan(N1, N2).

%%---------------------------------------------------------------------------
%% Internal Functions
%%---------------------------------------------------------------------------

-compile({inline,
          [ lessThan/2
          , greaterThan/2
          , lessThanOrEq/2
          , greaterThanOrEq/2
          ]}).
lessThan(X, Y) -> X < Y.
greaterThan(X, Y) -> X > Y.
lessThanOrEq(X, Y) -> X =< Y.
greaterThanOrEq(X, Y) -> X >= Y.


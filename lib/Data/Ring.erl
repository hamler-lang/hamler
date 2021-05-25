%%---------------------------------------------------------------------------
%% |
%% Module      :  Ring
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
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

-export([ intSub/2
        , floatSub/2
        , intDiv/2
        , floatDiv/2
        , intRem/2
        ]).

%% intSub :: Integer -> Integer -> Integer
-spec(intSub(integer(), integer()) -> integer()).
intSub(X, Y) -> X - Y.

%% floatSub :: Float -> Float -> Float
-spec(floatSub(float(), float()) -> float()).
floatSub(X, Y) -> X - Y.

%% intDiv :: Integer -> Integer -> Integer
-spec(intDiv(integer(), integer()) -> integer()).
intDiv(X, Y) -> X div Y.

%% floatDiv :: Float -> Float -> Float
-spec(floatDiv(float(), float()) -> float()).
floatDiv(X, Y) -> X / Y.

%% intRem :: Integer -> Integer -> Integer
-spec(intRem(integer(), integer()) -> integer()).
intRem(X, Y) -> X rem Y.

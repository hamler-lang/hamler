%%---------------------------------------------------------------------------
%% |
%% Module      :  Random
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Random FFI module.
%%
%%---------------------------------------------------------------------------
-module('Random').

-export([ randomCharImpl/1
        , randomRCharImpl/2
        , randomIntegerImpl/1
        , randomRIntegerImpl/2
        , randomFloatImpl/1
        , randomRFloatImpl/2
        ]).

randomCharImpl(C) ->
    rand:uniform(C).

randomRCharImpl(C1, C2) ->
    rand:uniform(C2 - C1) + C1.

randomIntegerImpl(I) ->
    rand:uniform(I).

randomRIntegerImpl(I1, I2) ->
    rand:uniform(I2 - I1) + I1.

randomFloatImpl(N) ->
    rand:uniform_real() * N.

randomRFloatImpl(N1, N2) ->
    N1 + (rand:uniform_real() * (N2 - N1)).

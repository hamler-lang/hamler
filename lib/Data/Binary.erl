%%---------------------------------------------------------------------------
%% |
%% Module      :  Binary
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Binary FFI module.
%%
%%---------------------------------------------------------------------------
-module('Binary').

-export([ eqBinImpl/2
        , cmpBinImpl/3
        ]).

-spec(eqBinImpl(binary(), binary()) -> boolean()).
eqBinImpl(B1, B2) -> B1 == B2.

cmpBinImpl(LT, EQ, GT) ->
    fun(B1, B2) when B1 < B2 -> LT;
       (B1, B2) when B1 == B2 -> EQ;
       (B1, B2) when B1 > B2 -> GT
    end.

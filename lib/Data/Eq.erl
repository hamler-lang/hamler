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
%% The Eq FFI module.
%%
%%---------------------------------------------------------------------------
-module('Eq').

-export([ eqAtomImpl/2
        , eqBoolImpl/2
        , eqBinImpl/2
        , eqCharImpl/2
        , eqIntImpl/2
        , eqFloatImpl/2
        , eqStringImpl/2
        , eqListImpl/2
        ]).

-spec(eqAtomImpl(atom(), atom()) -> boolean()).
eqAtomImpl(A1, A2) -> A1 =:= A2.

eqBoolImpl(B1, B2) -> B1 =:= B2.

-spec(eqBinImpl(binary(), binary()) -> boolean()).
eqBinImpl(B1, B2) -> B1 == B2.

eqCharImpl(C1, C2) -> C1 =:= C2.

eqIntImpl(I1, I2)  -> I1 =:= I2.

eqFloatImpl(N1, N2) -> N1 =:= N2.

eqStringImpl(S1, S2) -> S1 =:= S2.

eqListImpl(L1, L2) -> L1 =:= L2.

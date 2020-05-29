%%---------------------------------------------------------------------------
%% |
%% Module      :  Bits
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Bits FFI module.
%%
%%---------------------------------------------------------------------------
-module('Bits').

-export([ 'band'/2
        , 'bor'/2
        , 'bxor'/2
        , 'bnot'/1
        , 'bsl'/2
        , 'bsr'/2
        ]).

-spec('band'(integer(), integer()) -> integer()).
'band'(X, Y) -> X band Y.

-spec('bor'(integer(), integer()) -> integer()).
'bor'(X, Y)  -> X bor Y.

-spec('bxor'(integer(), integer()) -> integer()).
'bxor'(X, Y) -> X bxor Y.

-spec('bnot'(integer()) -> integer()).
'bnot'(X) -> bnot X.

-spec('bsl'(integer(), integer()) -> integer()).
'bsl'(X, Y) -> X bsl Y.

-spec('bsr'(integer(), integer()) -> integer()).
'bsr'(X, Y) -> X bsr Y.


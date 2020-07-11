%%---------------------------------------------------------------------------
%% |
%% Module      :  Foreign
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Foreign FFI module.
%%
%%---------------------------------------------------------------------------
-module('Foreign').

-include("Foreign.hrl").

-export([ ffi0/2
        , ffi1/3
        , ffi2/4
        , ffi3/5
        , ffi4/6
        , ffi5/7
        , ffi6/8
        , ffi7/9
        ]).

-export([ ffiIO0/2
        , ffiIO1/3
        , ffiIO2/4
        , ffiIO3/5
        , ffiIO4/6
        , ffiIO5/7
        , ffiIO6/8
        , ffiIO7/9
        ]).

%% Pure FFI
ffi0(Mod, Fun) -> Mod:Fun().
ffi1(Mod, Fun, A) -> Mod:Fun(A).
ffi2(Mod, Fun, A, B) -> Mod:Fun(A, B).
ffi3(Mod, Fun, A, B, C) -> Mod:Fun(A, B, C).
ffi4(Mod, Fun, A, B, C, D) -> Mod:Fun(A, B, C, D).
ffi5(Mod, Fun, A, B, C, D, E) -> Mod:Fun(A, B, C, D, E).
ffi6(Mod, Fun, A, B, C, D, E, F) -> Mod:Fun(A, B, C, D, E, F).
ffi7(Mod, Fun, A, B, C, D, E, F, G) -> Mod:Fun(A, B, C, D, E, F, G).

%% FFI with Effect
ffiIO0(Mod, Fun) -> ?IO(ffi0(Mod, Fun)).
ffiIO1(Mod, Fun, A) -> ?IO(ffi1(Mod, Fun, A)).
ffiIO2(Mod, Fun, A, B) -> ?IO(ffi2(Mod, Fun, A, B)).
ffiIO3(Mod, Fun, A, B, C) -> ?IO(ffi3(Mod, Fun, A, B, C)).
ffiIO4(Mod, Fun, A, B, C, D) -> ?IO(ffi4(Mod, Fun, A, B, C, D)).
ffiIO5(Mod, Fun, A, B, C, D, E) -> ?IO(ffi5(Mod, Fun, A, B, C, D, E)).
ffiIO6(Mod, Fun, A, B, C, D, E, F) -> ?IO(ffi6(Mod, Fun, A, B, C, D, E, F)).
ffiIO7(Mod, Fun, A, B, C, D, E, F, G) -> ?IO(ffi7(Mod, Fun, A, B, C, D, E, F, G)).

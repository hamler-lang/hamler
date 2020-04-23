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
%% The Foreign Module.
%%
%%---------------------------------------------------------------------------
-module('Foreign').

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
ffi0(Mod, Fun) -> ffiApply(Mod, Fun, []).
ffi1(Mod, Fun, A) -> ffiApply(Mod, Fun, [A]).
ffi2(Mod, Fun, A, B) -> ffiApply(Mod, Fun, [A, B]).
ffi3(Mod, Fun, A, B, C) -> ffiApply(Mod, Fun, [A, B, C]).
ffi4(Mod, Fun, A, B, C, D) -> ffiApply(Mod, Fun, [A, B, C, D]).
ffi5(Mod, Fun, A, B, C, D, E) -> ffiApply(Mod, Fun, [A, B, C, D, E]).
ffi6(Mod, Fun, A, B, C, D, E, F) -> ffiApply(Mod, Fun, [A, B, C, D, E, F]).
ffi7(Mod, Fun, A, B, C, D, E, F, G) -> ffiApply(Mod, Fun, [A, B, C, D, E, F, G]).

%% FFI with Effect
ffiIO0(Mod, Fun) -> ffi0(Mod, Fun).
ffiIO1(Mod, Fun, A) -> ffi1(Mod, Fun, A).
ffiIO2(Mod, Fun, A, B) -> ffi2(Mod, Fun, A, B).
ffiIO3(Mod, Fun, A, B, C) -> ffi3(Mod, Fun, A, B, C).
ffiIO4(Mod, Fun, A, B, C, D) -> ffi4(Mod, Fun, A, B, C, D).
ffiIO5(Mod, Fun, A, B, C, D, E) -> ffi5(Mod, Fun, A, B, C, D, E).
ffiIO6(Mod, Fun, A, B, C, D, E, F) -> ffi6(Mod, Fun, A, B, C, D, E, F).
ffiIO7(Mod, Fun, A, B, C, D, E, F, G) -> ffi7(Mod, Fun, A, B, C, D, E, F, G).

-compile({inline, [ffiApply/3]}).
ffiApply(Mod, Fun, Args) ->
    erlang:apply(list_to_atom(Mod), list_to_atom(Fun), Args).

%% curry(M, F, A, 0) -> erlang:apply(M, F, lists:reverse(A));
%% curry(M, F, A, N) -> fun(X) -> curry(M, F, [X|A], N-1) end.

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

-export([ffi/3, ffiIO/3]).

-type(arity() :: non_neg_integer()).

-spec(ffi(string(), string(), arity()) -> fun()).
ffi(Mod, Fun, Arity) ->
    M = a(Mod),
    F = a(Fun),
    N = Arity,
    fun M:F/N.

-spec(ffiIO(string(), string(), arity()) -> fun()).
ffiIO(Mod, Fun, Arity) ->
    ffi(Mod, Fun, Arity).

-compile({inline, [a/1]}).
a(S) -> list_to_existing_atom(S).

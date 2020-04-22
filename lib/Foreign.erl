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

-spec(ffi(string(), string(), arity()) -> fun()).
ffi(Mod, Fun, Arity) when Arity >= 0 ->
    curry(atom(Mod), atom(Fun), [], Arity).

-spec(ffiIO(string(), string(), arity()) -> fun()).
ffiIO(Mod, Fun, Arity) -> ffi(Mod, Fun, Arity).

curry(M, F, A, 0) -> erlang:apply(M, F, A);
curry(M, F, A, N) -> fun(X) -> curry(M, F, [X|A], N-1) end.

-compile({inline, [atom/1]}).
atom(S) -> list_to_existing_atom(S).

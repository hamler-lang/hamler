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

-export([ ffi/2
        , ffi/3
        , ffiIO/2
        , ffiIO/3
        ]).

-spec(ffi(string(), string()) -> fun()).
ffi(Mod, Fun) ->
    {Fun1, Arity} = split(Fun),
    ffi(Mod, Fun1, Arity).

-spec(ffi(string(), string(), arity()) -> fun()).
ffi(Mod, Fun, Arity) when Arity >= 0 ->
    curry(atom(Mod), atom(Fun), [], Arity).

-spec(ffiIO(string(), string()) -> fun()).
ffiIO(Mod, Fun) -> ffi(Mod, Fun).

-spec(ffiIO(string(), string(), arity()) -> fun()).
ffiIO(Mod, Fun, Arity) -> ffi(Mod, Fun, Arity).

curry(M, F, A, 0) -> erlang:apply(M, F, lists:reverse(A));
curry(M, F, A, N) -> fun(X) -> curry(M, F, [X|A], N-1) end.

-compile({inline, [split/1]}).
split(S) ->
    case string:split(S, "/") of
        [F, A] -> {F, int(A)};
        [F] -> {F, 0}
    end.

-compile({inline, [atom/1]}).
atom(S) -> list_to_atom(S).

-compile({inline, [int/1]}).
int(S) -> list_to_integer(S).

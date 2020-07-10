%%---------------------------------------------------------------------------
%% |
%% Module      :  Show
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Show FFI module.
%%
%%---------------------------------------------------------------------------
-module('Show').

-export([ showAtomImpl/1
        , showIntImpl/1
        , showFloatImpl/1
        , showNumImpl/1
        , showCharImpl/1
        , showAny/1
        ]).

%% Atom -> String
-spec(showAtomImpl(atom()) -> string()).
showAtomImpl(A) -> atom_to_list(A).

%% Integer -> String
-spec(showIntImpl(integer()) -> string()).
showIntImpl(I) -> integer_to_list(I).

%% Float -> String
-spec(showFloatImpl(float()) -> string()).
showFloatImpl(F) ->
   erlang:float_to_list(F, [{decimals,precision(abs(F), 0)}]).

%% Float -> String
-spec(showNumImpl(number()) -> string()).
showNumImpl(N) when is_integer(N) ->
    showIntImpl(N);
showNumImpl(N) when is_float(N) ->
    showFloatImpl(N).

%% showChar :: Char -> String
-spec(showCharImpl(char()) -> string()).
showCharImpl(C) -> [C].

precision(A, P) ->
    case A == trunc(A) of
        true  -> P;
        false -> precision(A*10.0, P+1)
    end.

-spec(showAny(any()) -> string()).
showAny(A) -> lists:flatten(io_lib:format("~p", [A])).

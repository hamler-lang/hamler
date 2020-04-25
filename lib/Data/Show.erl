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
%% The Show Module.
%%
%%---------------------------------------------------------------------------
-module('Show').

-export([ showIntImpl/1
        , showFloatImpl/1
        , showNumImpl/1
        , showCharImpl/1
        ]).

%% showInt :: Integer -> String
-spec(showIntImpl(integer()) -> string()).
showIntImpl(I) -> integer_to_list(I).

%% showFloat :: Number -> String
-spec(showFloatImpl(float()) -> string()).
showFloatImpl(F) ->
   erlang:float_to_list(F, [{decimals,precision(abs(F), 0)}]).

%% showNumber :: Number -> String
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

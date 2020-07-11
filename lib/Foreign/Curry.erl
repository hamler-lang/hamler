%%---------------------------------------------------------------------------
%% |
%% Module      :  Curry
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Curry module.
%%
%%---------------------------------------------------------------------------
-module('Curry').

-export([uncurry/2, uncurryIO/2]).

uncurryIO(Fun, [H|T]) -> uncurryIO(Fun(H), T);
uncurryIO(Ret, []) -> Ret().

uncurry(Fun, [H|T]) -> uncurry(Fun(H), T);
uncurry(Ret, []) -> Ret.

%%curry(M, F, A, 0) -> erlang:apply(M, F, lists:reverse(A));
%%curry(M, F, A, N) -> fun(X) -> curry(M, F, [X|A], N-1) end.


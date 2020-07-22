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

-include("../Foreign.hrl").

-compile({no_auto_import, [apply/2]}).

-export([ curry/1
        , curryIO/1
        , apply/2
        , applyIO/2
        ]).

curryIO(Fun) -> ?IO(curry(Fun)).

%% Curry a function
curry(Fun) -> curry(Fun, arity(Fun)).

curry(Fun, 0) -> Fun; %% ignore
curry(Fun, 1) -> Fun; %% ignore
curry(Fun, N) -> curry(Fun, [], N).

curry(Fun, Args, 0) ->
  erlang:apply(Fun, lists:reverse(Args));
curry(Fun, Args, N) ->
  fun(X) -> curry(Fun, [X|Args], N-1) end.

applyIO(Fun, Args) ->
  apply(?RunIO(Fun), Args).

%% Apply a curried function
apply(Fun, [A|Args]) -> apply(Fun(A), Args);
apply(Ret, []) -> Ret.

arity(Fun) ->
  element(2, erlang:fun_info(Fun, arity)).

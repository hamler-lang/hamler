%%---------------------------------------------------------------------------
%% |
%% Module      :  Monad
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Monad FFI module.
%%
%%---------------------------------------------------------------------------
-module('Monad').

-export([ applyListImpl/2
        , bindImpl/2
        , bindListImpl/2
        , pureImpl/1
        , seqio/1
        , seqio_/1
        , unsafePerformIO/1
        , replApply/1
        ]).

-type(mapFun() :: fun((A :: any()) -> B :: any())).

-spec(applyListImpl(list(mapFun()), list(any())) -> list(any())).
applyListImpl(Funs, L) ->
    [F(X) || X <- L, F <- Funs].

-spec(bindImpl(any(), fun((A :: term()) -> B :: term())) -> any()).
bindImpl(X, F) -> fun() -> (F(X()))() end.

-spec(bindListImpl(list(term()), fun((term()) -> list(term()))) -> list(term())).
bindListImpl(L, F) ->
    lists:append(lists:map(F, L)).

-spec(pureImpl(any()) -> any()).
pureImpl(X) -> fun() -> X end.

seqio(L) when is_list(L) -> fun() -> [V() || V <- L] end.

seqio_(L) when is_list(L) -> 
  fun() -> [V() || V <- L],
   ok end.

unsafePerformIO(L) -> L().

replApply(F) ->
  case is_function(F) andalso (element(2, erlang:fun_info(F, arity)) == 0) of
    true -> F();
    false -> F
  end.

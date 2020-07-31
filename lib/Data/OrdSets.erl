%%---------------------------------------------------------------------------
%% |
%% Module      :  OrdSet
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%%                Rory Z, rory@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The OrdSet FFI module.
%%
%%---------------------------------------------------------------------------
-module('OrdSet').

-type(ordset(T) :: [T]).

-export([fold/3]).

%% forall a acc. (a -> acc -> acc) -> acc -> OrdSet a -> acc
-spec(fold(Function, Acc0, Ordset) -> Acc1 when
      Function :: fun((Element :: T, AccIn :: term()) -> AccOut :: term()),
      Ordset :: ordset(T),
      Acc0 :: term(),
      Acc1 :: term()).
fold(Fun, Acc0, Ordset) ->
  ordsets:fold(fun(Element, AccIn) ->
                 'Curry':apply(Fun, [Element, AccIn])
               end, Acc0, Ordset).

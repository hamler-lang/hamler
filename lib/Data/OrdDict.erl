%%---------------------------------------------------------------------------
%% |
%% Module      :  OrdDict
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%%                Rory Z, rory@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The OrdDict FFI module.
%%
%%---------------------------------------------------------------------------
-module('OrdDict').

-include("../Foreign/Maybe.hrl").

-compile({no_auto_import, [apply/2]}).

-export([ filter/2
        , find/2
        , fold/3
        , take/2
        , map/2
        , merge/3
        ]).

-import('Curry', [apply/2]).

-type(orddict(Key, Value) :: [{Key, Value}]).

%% filter :: forall a b. (a -> b -> Boolean) -> OrdDict a b -> OrdDict a b
-spec(filter(Pred, Orddict1) -> Orddict2 when
      Pred :: fun((Key, Value) -> boolean()),
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value)).
filter(Pred, Orddict1) ->
  orddict:filter(fun(Key, Value) ->
                     apply(Pred, [Key, Value])
                 end, Orddict1).

%% find :: forall a b. a -> OrdDict a b -> Maybe b -- {Ok, b} | Error
-spec(find(Key, Orddict) -> maybe(Value) when
      Orddict :: orddict(Key, Value)).
find(Key, Orddict) ->
  case orddict:find(Key, Orddict) of
    {ok, Value} -> ?Just(Value);
    error -> ?Nothing
  end.

%% fold :: forall a b acc. (a -> b -> acc -> acc)  -> acc -> OrdDict a b -> acc
-spec(fold(Fun, Acc0, Orddict) -> Acc1 when
      Fun :: fun((Key, Value, AccIn) -> AccOut),
      Orddict :: orddict(Key, Value),
      Acc0 :: Acc,
      Acc1 :: Acc,
      AccIn :: Acc,
      AccOut :: Acc).
fold(Fun, Acc0, Orddict) ->
  orddict:fold(fun(Key, Value, AccIn) ->
                   apply(Fun, [Key, Value, AccIn])
               end, Acc0, Orddict).

%% take :: forall a b. a -> OrdDict a b -> Maybe b (OriDict a b)
-spec(take(Key, Orddict) -> maybe({Value, Orddict1 }) when
      Orddict :: orddict(Key, Value),
      Orddict1 :: orddict(Key, Value),
      Key :: term(),
      Value :: term()).
take(Key, Orddict) ->
  case orddict:take(Key, Orddict) of
    {Value, Ordrdict1} ->
      ?Just({Value, Ordrdict1});
    error -> ?Nothing
  end.

%% map :: forall a b. (a -> b -> b) -> OrdDict a b -> OrdDict a b
-spec(map(Fun, Orddict1) -> Orddict2 when
      Fun :: fun((Key, Value1) -> Value2),
      Orddict1 :: orddict(Key, Value1),
      Orddict2 :: orddict(Key, Value2)).
map(Fun, Orddict1) ->
  orddict:map(fun(Key, Value1) ->
                  apply(Fun, [Key, Value1])
              end, Orddict1).

%% merge :: forall a b. (a -> b -> b -> b) -> OrdDict a b -> OrdDict a b -> OrdDict a b
-spec merge(Fun, Orddict1, Orddict2) -> Orddict3 when
      Fun :: fun((Key, Value1, Value2) -> Value),
      Orddict1 :: orddict(Key, Value1),
      Orddict2 :: orddict(Key, Value2),
      Orddict3 :: orddict(Key, Value).
merge(Fun, Orddict1, Orddict2) ->
  orddict:merge(fun(Key, Value1, Value2) ->
                    apply(Fun, [Key, Value1, Value2])
                end, Orddict1, Orddict2).

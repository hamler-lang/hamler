%%---------------------------------------------------------------------------
%% |
%% Module      :  Map
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Map FFI module.
%%
%%---------------------------------------------------------------------------
-module('Map').

-include("../Foreign/Maybe.hrl").

-export([ singleton/2
        , isEmpty/1
        , lookup/2
        , notMember/2
        , take/2
        , filter/2
        , eqMapImpl/2
        , mapWithKey/2
        ]).

%% forall k v. k -> v -> Map k v
singleton(K, V) -> #{K => V}.

%% isEmpty :: forall k v. Map k v -> Boolean
-spec(isEmpty(map()) -> boolean()).
isEmpty(Map) -> maps:size(Map) == 0.

%% lookup :: forall k v. k -> Map k v -> Maybe v
-spec(lookup(Key :: term(), map()) -> maybe(Value :: term())).
lookup(Key, Map) ->
  case maps:find(Key, Map) of
    {ok, Value} -> ?Just(Value);
    error -> ?Nothing
  end.

-spec(notMember(Key :: term(), map()) -> boolean()).
notMember(Key, Map) -> not maps:is_key(Key, Map).

-spec(take(Key :: term(), map()) -> maybe({Value :: term(), map()})).
take(Key, Map) ->
  case maps:take(Key, Map) of
    {Value, Map2} -> ?Just({Value, Map2});
    error -> ?Nothing
  end.

filter(Fun, Map) ->
    maps:filter(fun(K, V) -> Fun({K,V}) end, Map).

eqMapImpl(Map1, Map2) ->
     Map1 == Map2.

mapWithKey(Fun, Map) ->
    maps:map(fun(K, V) -> Fun({K,V}) end, Map).

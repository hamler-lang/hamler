%%---------------------------------------------------------------------------
%% |
%% Module      :  Map
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Map FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Map').

-export([ empty/0
        , isEmpty/1
        , insert/3
        , lookup/2
        , delete/2
        , updateWith/3
        , size/1
        , filter/2
        ]).

-type(pred() :: fun((Key :: term(), Value :: term()) -> boolean())).

-type(update() :: fun((Value1 :: term()) -> Value2 :: term())).

%% empty   :: forall k v.Ord k => Map k v
-spec(empty() -> map()).
empty() -> #{}.

%% isEmpty :: forall k v.Ord k => Map k v -> Boolean
-spec(isEmpty(map()) -> boolean()).
isEmpty(Map) -> maps:size(Map) == 0.

%% insert  :: forall k v.Ord k => k -> v -> Map k v -> Map k v
-spec(insert(Key :: term(), Value :: term(), map()) -> map()).
insert(Key, Value, Map) -> maps:put(Key, Value, Map).

%% lookup  :: forall k v.Ord k => k -> Map k v -> Maybe v
-spec(lookup(Key :: term(), map()) -> {'Just', term()} | 'Nothing').
lookup(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> {'Just', Value};
        error -> 'Nothing'
    end.

%% delete  :: forall k v.Ord k => k -> Map k v -> Map k v
-spec(delete(Key :: term(), map()) -> map()).
delete(Key, Map) -> maps:delete(Key, Map).

%% update  :: forall k v.Ord k => (v -> Maybe v) -> k -> Map k v -> Map k v
-spec(updateWith(Key :: term(), update(), map()) -> map()).
updateWith(Key, Fun, Map) -> maps:update_with(Key, Fun, Map).

%% size    :: forall k v.Ord k => Map k v -> Int
-spec(size(map()) -> integer()).
size(Map) -> maps:size(Map).

%% filter  :: forall k v.Ord k => (v -> Boolean) -> Map k v -> Map k v
-spec(filter(pred(), map()) -> map()).
filter(Pred, Map) -> maps:filter(Pred, Map).


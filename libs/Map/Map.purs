module Map where

import Maybe

foreign import data Map :: Type -> Type -> Type

foreign import empty :: forall k v. Map k v
foreign import isEmpty :: forall k v. Map k v -> Boolean
foreign import insert :: forall k v.k -> v -> Map k v -> Map k v
foreign import lookup :: forall k v. k -> Map k v -> Maybe v
foreign import delete :: forall k v.k -> Map k v -> Map k v
foreign import update :: forall k v. (v -> Maybe v) -> k -> Map k v -> Map k v
foreign import size :: forall k v. Map k v -> Int
foreign import filter :: forall k v.(v -> Boolean) -> Map k v -> Map k v







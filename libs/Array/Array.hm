module Array where

import Maybe
foreign import singleton :: forall a.a -> Array a
foreign import range :: Int -> Int -> Array Int
foreign import replicate :: forall a.Int -> a -> Array a
foreign import null :: forall a.Array a -> Boolean
foreign import length :: forall a.Array a -> Int
foreign import cons :: forall a.a -> Array a -> Array a
foreign import snoc :: forall a.Array a -> a -> Array a
foreign import head :: forall a.Array a -> Maybe a
foreign import last :: forall a. Array a -> Maybe a
foreign import tail :: forall a. Array a -> Maybe (Array a)
foreign import init :: forall a. Array a -> Maybe (Array a)
foreign import index :: forall a. Array a -> Int -> Maybe a
foreign import insertAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
foreign import deleteAt :: forall a. Int -> Array a -> Maybe (Array a)
foreign import updateAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
foreign import reverse :: forall a. Array a -> Array a
foreign import filter :: forall a. (a -> Boolean) -> Array a -> Array a
foreign import sort :: forall a. Array a -> Array a
foreign import take :: forall a. Int -> Array a -> Array a
foreign import map :: forall a b.(a -> b) -> Array a -> Array b




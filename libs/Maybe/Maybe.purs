module Maybe where

import Prelude

data Maybe a = Nothing | Just a

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a


fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe a = maybe a identity

isJust :: forall a. Maybe a -> Boolean
isJust = maybe false (const true)

isNothing :: forall a. Maybe a -> Boolean
isNothing = maybe true (const false)





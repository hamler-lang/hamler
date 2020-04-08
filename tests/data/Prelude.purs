module Prelude where

infixr 0 apply as $


apply :: forall a b. (a -> b) -> a -> b
apply f x = f x


k x y z = x 






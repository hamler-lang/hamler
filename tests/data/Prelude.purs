module Prelude where


infixl 6 add as +
infixl 6 sub as -
infixl 4 greaterThan as >
infixl 4 lessThan as <
infixr 0 apply as $

foreign import  add :: Int -> Int -> Int
foreign import  sub :: Int -> Int -> Int
foreign import  greaterThan :: Int -> Int -> Boolean
foreign import  lessThan :: Int -> Int -> Boolean



apply :: forall a b. (a -> b) -> a -> b
apply f x = f x









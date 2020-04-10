module List where

import Prelude
infixr 6 Cons as :

data List a = Nil | Cons a (List a)

nil :: forall a.List a
nil = Nil

cons :: forall a.a -> List a -> List a
cons a Nil = a : Nil
cons a xs = a :xs

map :: forall a b.(a->b) -> List a -> List b
map f Nil = Nil
map f (x:xs) = f x : map f xs

length :: forall a.List a -> Int
length Nil = 0
length (x:xs) = 1 + length xs

foldl :: forall a b.(b -> a -> b) -> b -> List a -> b
foldl f b Nil = b
foldl f b (x:xs) = foldl f (f b x) xs

foldr :: forall a b.(a -> b -> b) -> b -> List a -> b
foldr f b Nil = b
foldr f b (x:xs) = f x (foldr f b xs)











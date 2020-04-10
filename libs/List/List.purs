module List where

import Prelude
import Maybe
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

scanl :: forall a b.(b -> a -> b) -> b -> List a -> List b
scanl f b Nil = Nil
scanl f b (x:xs) = t : scanl f t xs
  where t = f b x

scanr :: forall a b.(a -> b -> b) -> b -> List a -> List b
scanr f b Nil = Nil
scanr f b (x:xs) = f x t : r
  where r = scanr f b xs
        t = case r of
              Nil -> b
              c:cs -> c

head ::forall a.List a -> Maybe a
head Nil = Nothing
head (x:xs) = Just x

last :: forall a.List a -> Maybe a
last Nil = Nothing
last (x:Nil) = Just x
last (x:xs) = last xs

reverse :: forall a.List a -> List a
reverse Nil = Nil
reverse (x:xs)= reverse xs ++  (x:Nil)


concat :: forall a.List (List a) -> List a
concat xs = foldr (\x y -> foldr (:) y x ) Nil xs




filter :: forall a. (a -> Boolean) -> List a -> List a
filter f Nil = Nil
filter f (x:xs)  = if f x
                   then  x : filter f xs
                   else filter f xs


infixl 6 connect as ++

connect ::forall a.List a -> List a -> List a
connect Nil ys = ys
connect (x:xs) ys = x : connect xs ys

and :: List Boolean -> Boolean
and Nil = true
and (x:xs) = x && and xs

or :: List Boolean -> Boolean
or Nil = false
or (x:xs) = x || or xs

any :: forall a.(a -> Boolean) -> List a -> Boolean
any f Nil = false
any f (x:xs) = f x || any f xs

all :: forall a.(a -> Boolean) -> List a -> Boolean
all f Nil = true
all f (x:xs) = f x && all f xs

replicate :: forall a. Int -> a -> List a
replicate i a | i < 0 = Nil
              | i == 0 =Nil
              | otherwise = a : replicate (i-1) a

take :: forall a. Int -> List a -> List a
take _ Nil = Nil
take 0 xs  = Nil
take i (x:xs) = x : take (i-1) xs

drop :: forall a. Int -> List a -> List a
drop _ Nil = Nil
drop 0 xs =xs
drop i (x:xs) = drop (i-1) xs


-------------example------------
createN :: Int -> List Int
createN 0 = Nil
createN i = i : createN (i-1)

t1 = createN 10
t2 = scanl (+) 0 t1
t3 = scanr (+) 0 t1

t4 = filter t t1
  where t x = false

t5 = filter (\x -> x > 4) t1
t6 = reverse t1
t7 = map createN (createN 5)
t8 = concat t7
t9 = and (true:true:true:Nil)
t10 = replicate 10 "nice"
t11 = take 5 t10
t12 = drop 8 t10
--------------------------------






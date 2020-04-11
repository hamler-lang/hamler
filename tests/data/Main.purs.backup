module Main (List(..),reverse,createN,createN2,fib,fibList,tife,list) where

import Prelude

-- | 暂时还不支持部分函数、、

infixr 8 List as :

data List = Nil | List Int List

create :: Int -> List
create 0 = Nil
create i = i : create (i-1)

reverse0 :: List -> List -> List
reverse0 Nil xs  = xs
reverse0 (a:b) xs = reverse0 b (a:xs)

reverse :: List -> List
reverse xs = reverse0 xs Nil

createN :: Int -> List
createN i = reverse (create i)

infixl 6 con as ++

con :: List -> List -> List
con Nil xs = xs
con (a:b) xs = a : (con b xs)

createN2 :: Int -> List
createN2 i = createN i ++ createN i

map :: (Int -> Int ) -> List -> List
map f Nil = Nil
map f (a:b) = f a : map f b

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)

fibList :: Int ->  List
fibList i = map fib $ createN i

zipWith :: (Int -> Int ->Int) -> List -> List -> List
zipWith f Nil ys = ys
zipWith f xs Nil = xs
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


tail :: List -> List
tail Nil = Nil
tail (x:xs) = xs


ife :: Int -> Int
ife x | x > 8 = 4
      | x > 6 = 3
      | x > 4 = 2
      | x > 2 = 1
      | true = x

tife :: List
tife = map ife (createN 40)


list = [1,2,3,4,5]

--fib1 :: Int -> List
--fib1 x = 1 : 1 : zipWith (+) (fib1 0) (tail (fib1 0))


















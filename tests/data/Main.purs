module Main  where

import Prelude


foreign import test1 :: Int -> Int -> Int
foreign import test2 :: Int -> Int -> Int

t = test1 1 2

t0 x = x 

t1 = t0 $  1 


t2 = k 1 2 3










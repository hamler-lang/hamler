module Main where

data T = T {
    age :: Int
  , name :: String
  , position :: String
  , fun :: Int -> Int
  , part :: G
  }

data G  = G {
    x :: Int
  , y :: Int
            }
ng = G 1 20
f g x =g (g x)

double x = x * 2

t = T 1 "yang" "hangzou" f ng

f1 = t.fun double 4
f2 = t.position
f3 = t.part.y
f4 x = if x > 10
        then t.age
        else t.part.x







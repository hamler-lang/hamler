module A where

-- import Prelude

-- Boolean literal
boolean :: Boolean
boolean = true

-- Int literal
integer :: Int
integer = 7

-- Number literal
number :: Number
number = 1.2

-- Char literal
character :: Char
character = 't'

-- String literal
string :: String
string = "thran"

-- Array literal
array :: Array Int
array = [1, 2, 3]

-- empty Record literal
record :: {}
record = {}

-- non-empty Record literal
nonEmpty :: { name :: String }
nonEmpty = { name: "thran" }

-- record access
getName :: { name :: String } -> String
getName person = person.name

-- case expression
switch :: Int -> Int
switch x = case x, x of
  0, 0 -> 0
  1, z -> z
  y, 1 -> y
  _, _ -> x

-- conditional expression
not :: Boolean -> Boolean
not x = if x
  then false
  else true


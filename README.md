
# Hamler

The Hamler Programming Language.

## Features

- Functional Programming
- Haskell and ML like
- ADT and Type inference
- Currying and Partial Application
- Pattern Match
- List Comprehension
- Recursion
- Module

## Syntax

```
-- Inline comment
{- block comment -}

{-- Variables --}

a = 1
let b = 2
let c = 1 + 2

{-- Basic Types --}

True :: Bool
False :: Bool
2 :: Integer
1.0 :: Float
3.14 :: Double
"hello" :: String
[1,2,3] -- List
(1, "a", True) -- Tuple

{- Algebraic Types -}

type Name = String

-- sum type
type Color = Red | Green | Blue
col = Red

-- product type with two fields
type Pair = Pair Integer Integer
p = Pair 7 11

-- record product type
type Person = Persion {
  name :: String
  age :: Integer
}

-- generic type
type Pair a b = Pair a b
p = Pair "hi", 10

-- recursive type
type BinaryTree = Leaf Integer | Tree BinaryTree BinaryTree

{- Arithmetic Operators -}

{- Logical Operators: && || not -}

{- Relational Operators: == /= < > <= >= -}

{- List Comprehensions -}

[x*2 | x <- [1,2,3]] -- [2,4,6]

{- Functions -}

average :: Num -> Num -> Num
average a b = (a + b) / 2.0

average 1 2 + 3
average 1 (2 + 3)

{- Execution Control -}

-- if then else
if x > 0
  then printLn "Hello"
  else printLn "World"

-- case of and pattern match

rgb = Red
case rgb of
  Red   -> "red"
  Green -> "green"
  Blue  -> "blue"

rgb = Green
case rgb of Red -> "red"; _ -> "not red"

-- Main

main = printLn "Hello World"

```

## Examples

```
module Example where

type Bool = True | False

let a = 1
let b = 2

add :: Int -> Int -> Int
add x y = x + y

main = printLn "hello world"
```

## Author

Feng Lee <feng@emqx.io>


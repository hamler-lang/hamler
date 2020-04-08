
# The Hamler Programming Language

`Hamler` is a functional programming language inspired by Haskell and Stardard ML, that compiles to Core Erlang.

## Why Hamler?

- A Functional programming language;
- With compile-time type checking and type inference(?);
- Compiles to Core Erlang, and running on the Erlang VM

## Features

- Functional Programming
- Haskell and Standard ML like
- ADT and Type Checking/Inference
- Currying and Partial Application
- Pattern Matching
- Guards, or conditional matches
- List comprehension
- Recursion
- Modules

## Basic Syntax

### Comments

```haskell
-- A single line comment
{- Multi-line comments -}
```

### Values, Types and Variables

```haskell
{-- Types, Values --}
True :: Bool
False :: Bool
2 :: Int
1.0 :: Float
3.14 :: Double
"hello" :: String

{-- Variables --}
a = 1
let b = 2
let s = "hello"
```

## Basic Types

A type is a set of values.

| Type              | Values        | Description                   |
| ----------------- | ------------- | ----------------------------- |
| None(???)         | None          | null                          |
| Bool              | True \| False | Boolean type                  |
| Atom(Symbol)      | :a, :b        |                               |
| Char              | 'c', 'x'      |                               |
| Int(Integer)      | 1, 2, -10     | Integer type                  |
| Float(Double)     | 3.14          | Float type                    |
| Num(Number)       | Int \| Float  |                               |
| String            | "hello"       | String is a list of character |
| Tuple             |               |                               |
| List              |               |                               |
| Enum, Range       |               |                               |
| Binary/Bitstrings |               | Erlang Bitstrings             |
| Map(Dict)         |               | Erlang Map                    |
| Record            |               |                               |
| Fun               |               | Function                      |
| Port              |               | Erlang Port                   |
| Pid               |               | Erlang Pid                    |
| Ref               |               | Erlang Reference              |

### Booleans

```haskell
True | False
```

### Numbers

Two types of numeric literals: integers and floats.

```haskell
-- Integer
1,2,-10

-- binary, octal, and hex literals
0x1, 0X1, 0x2a, 0X2A
0o1, 0O1, 0o52, 0O52
0b10, 0B10

-- floats and doubles
1.0, 1e10
2.3
2.3e-3
0.0023
```

### Atoms

```
:atom, :ok, :error
```

### Strings

```haskell
"Hello, World!"
printf "foo %s %d %.2f" "bar" 7 3.1415

-- Multi-line Strings?
s = "My long \
\string"

-- Escape Codes
"\r\n ..."

-- ++ to concat strings
"Hello " ++ " World"
```

### Bitstrings/Binaries

```erlang
<<1,2,3>>
<<"ABC">>
<<1:16,2:4,3:4>>
```

### Tuples

A tuple is a sequence of values of different types:

```haskell
{- Tuple -}
(1, "a", True)
(1, "a")

-- fst, snd
fst (1, 'a') -- 1
snd (1, 'a') -- 'a'
```

### Lists

A list is sequence of values of the same type:

```haskell
{- List --}
[] -- empty list
[1,2,3] -- Integer list
[x:xs] -- Cons operator
[1:[2,3]] -- Cons
[1:[2:[3:[]]]] -- Cons

[x:_]  -- List pattern
[_:xs] -- List pattern
```

### Enumerations, Range

```haskell
{- Enumerations, Range -}
[1..10]
[1, 3..100]
['a'..'z']
```

### Maps

```haskell
-- New map
m = #{"foo" => "bar", "bar" => "foo"}
-- Match Map
#{"foo" := a, "bar" := b} = m
-- get, put
Map.get "foo" m -- a = "bar"
Map.get "bar" m -- b = "foo"
m1 = Map.put "key" "val"
-- keys, values
let keys = Map.keys m
let values = Map.values m
```

### Records

```haskell
-- declare a Person record
data Person = Person {name :: String, age :: Int}

-- or
data Person = {name :: String, age :: Int}

-- create a Person record
p = Person {name = "John", age = 12}

-- update a Person record
p1 = p {name = "Miles", age = 20}

-- getters
let name = p1.name
let age = p1.age
```

### Ports

Erlang port identifier identifies an Erlang port.

### PIDs

Erlang process identifier, pid, identifies a process.

## User-defined Types

```haskell
-- type synonym
type Name = String
n = "Miles" :: Name

-- sum datatype
data Color = Red | Green | Blue
c = Blue

-- pattern match sum type
case c of
  Red -> "red"
  Green -> "green"
  Blue -> "blue"
  _ -> "unknown"

-- product datatype
data Pair = Pair Int Int
p = Pair 3 4

-- record product datatype
data Person = Person {
  name :: String
  age :: Int
  address :: String
}
person = Person {name = "Miles", age = 50, address = "NY"}

-- generic datatype (maybe for example)
data Maybe a = Just a | None
data Result val err = Ok Val | Error err

-- recursive datatype
data Tree = Leaf Int | Node Tree Tree
```

## Functions

A function is a mapping from values of one type to values of another type.

### Function Definition

```haskell
{- Functions -}
sum :: Int -> Int -> Int
sum a b = a + b
-- or let...
let sum a b = a + b
```

### Function Application

```haskell
sum 1 2
sum 1 (2 + 3)
```

### Function Composition

```haskell
f x = x + 2
g x = x * 3
z = f (g 4) -- 14
```

### High-Order Functions

```haskell
apply :: (a -> a) -> a -> a
apply f x = f x
```

### Recursive Function

```haskell
fact n = if n == 0 then 1 else n * fact(n-1)
-- tail recursive
loop 0 = 0
loop n = loop (n - 1)
```

### Currying and Partial Application

```haskell
plus2 = (+) 2
plus2 3 -- 5
```

### Polymorphic Functions

```haskell
length :: [a] -> Int
zip :: [a] -> [b] -> [(a,b)]
```

### Lambda (Anonymous Function)

```erlang
\x -> \y -> (x + y) div 2
multBy n = \m -> n * m
```

### Guarded Equations

```haskell
abs n | n > 0     = n
      | otherwise = -n
```

## Expressions

### let

```haskell
let n = 1 + 2
let absn = if n < 0 then -n else n
```

### let .. in ..

```haskell
z = let x = 3
        y = 2 * x
    in x * y
```

### where

```haskell
z = x * y
    where
      x = 3
      y = 5
```

### case .. of

```haskell
-- case of and pattern match
rgb = Red
case rgb of
  Red   -> "red"
  Green -> "green"
  Blue  -> "blue"

rgb = Green
case rgb of Red -> "red"; _ -> "not red"
```

### if .. then .. else

```haskell
-- if then else
if x > 0 then x else -x

-- indent
if x > 0
  then print "pos"
  else if x < 0
    then print "neg"
    else print "zero"
```

### List comprehension

A list comprehension consists of four types of elements: *generators*, *guards*, *local bindings*, and *targets*.

```haskell
squares = [x * x | x <- [1..]]

-- multiple generators
[(x,y) | x <- [1,2,3], y <- [4,5]]

-- dependent generators
[(x,y) | x <- [1..3], y <- [x..3]]

-- guards
[x | x <- [1..10], even x]
```

```haskell
{- List Comprehensions -}

[x*2 | x <- [1,2,3]] -- return [2,4,6]
```

### Pattern Matching

```haskell
(x, y) = (1, 2)

-- function declare via pattern matching
allEmpty [] = True
allEmpty _ = False
```

### Guards

```haskell
which n
  | n == 0 = "zero!"
  | even n = "even!"
  | otherwise = "odd!"
```

### Statement terminator

*TODO: next line has equal or less indentation, or* ;

### Blocks

TODO: begin *expr* ; *…* end

## Operators

### Arithmetic Operators

| Operator | Name             | Example |
| -------- | ---------------- | ------- |
| +        | Add              |         |
| -        | Subtract         |         |
| *        | Multiply         |         |
| /        | Divide           |         |
| div      | Integer Division | div 7 3 |
| rem      | Remain           | rem 7 3 |
|          |                  |         |

### Logical Operators

| Operator | Name     |
| -------- | -------- |
| &&, and  | And      |
| \|\|, or | Or       |
| not      | Not      |
| andalso  | And also |
| orelse   | Or else  |

### Relational Operators

| Operator | Name        |
| -------- | ----------- |
| ==       | Equal       |
| /=       | Not Equal   |
| <        | Less        |
| >        | Great       |
| <=       | Less Equal  |
| >=       | Great Equal |

### Bit Operators

| BitOp | Name            |
| ----- | --------------- |
| band  | Bit and         |
| bor   | Bit or          |
| bnot  | Bit not         |
| bxor  | Bit xor         |
| bsl   | Bit shift left  |
| bsr   | Bit shift right |

## Modules

A module is a compilation unit which exports types, functions, and other modules.

### Module Declaration

A module name must start with a capital letter.

```haskell
-- Declare a module and export all the types and functions
module MyMod where

-- Declare a module and export some types or functions
module MyMod (Maybe(..), add) where

data Maybe a = Just a | Nothing

add :: Num -> Num -> Num
add x y = x + y
```

### Main

```haskell
-- Main
module Main (main) where

main = println "Hello World"
```

### Export

### Import

```haskell
import Data.List
import Data.Map (keys, values)

nth 1 [1..10]
keys #{"key" => "val"} -- ["key"]
values #{"key" => "val"} -- ["val"]

-- Qualified Imports
import Data.Set as Set
import Data.Map as Map

Map.get "foo" #{"foo" => "bar"}
```

## Libraries

### Prelude

The module which is always imported.

| Function | Example |
| -------- | ------- |
| min      | min 1 2 |
| max      | max 1 2 |
|          |         |

### List

TODO

### Math

```haskell
sqrt power exp log sin cos tan asin acos atan atan2
truncate round floor ceiling
```

### Date and Time

## Effect

TODO: How to handle side effects? Monad??

## IO

TODO...

## Examples

### Hello World

```haskell
module Example (add) where

add :: Int -> Int -> Int
add x y = x + y

main = printLn "hello world"
```

## Reserved Words

```haskell
and andalso band bor bxor bnot bsl bsr begin case class data do else end export if import in let of module not orelse then type
```

## Author

- Feng Lee <feng@emqx.io>
- Yang Miao <yangm@emqx.io>


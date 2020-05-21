# The Hamler Programming Language

`Hamler` is a functional programming language that allows you to write Erlang in Haskell's Style. 

## Why Hamler?

- A functional programming language;
- Type System in progress;
- Compiles to Core Erlang, and running on the Erlang VM;

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
-- Types, Values
true :: Boolean
false :: Boolean
2 :: Integer
1.0 :: Float
'a' :: Char
"hello" :: String

-- Variables Binding 
a = 1
```

## Local Bindings 

### let

```Haskell
do
  let n = 1 + 2
  let absn = if n < 0 then -n else n
  ...
```

```haskell
 $ repl 
 > let x = 1
 > x
 1
```

### let .. in ..

```haskell
z = let x = 3
        y = 2 * x
    in  x * y
```

### where

```haskell
z = x * y
    where
      x = 3
      y = 5
```



## Basic Types

A type can be seen as the set of some values.

| Type              | Values        | Description                   |
| ----------------- | ------------- | ----------------------------- |
| Bool              | true \| false | Boolean type                  |
| Atom(Symbol)      | :a, :b        |                               |
| Char              | 'c', 'x'      |                               |
| Integer(Int)      | 1, 2, -10     | Integer type                  |
| Float(Double)     | 3.14          | Float type                    |
| Number(Num)       | 1, 3.14, -2   | Integer \| Float type         |
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
true | false
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

-- floats
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
(1, "a", True)
(1, "a")

-- fst, snd
fst (1, 'a') :: Integer -- 1
snd (1, 'a') :: Char    -- 'a'
```

### Lists

A list is sequence of values of the same type:

```haskell
[] -- empty list
x : xs -- Cons operator meaning that x is put on the front of list xs

[1,2,3] -- Integer list

{-
[1,2,3] can be desugared to 
(1:(2:(3:[])))
-}

```

**List comprehension**

A list comprehension consists of four types of elements: *generators*, *guards*, *local bindings*, and *targets*.

```haskell
-- Examples
double = [x*2 | x <- [1,2,3]]      -- double = [2,4,6]

squares = [x * x | x <- [1..]]     -- This is an infinite list

-- Multiple generators
[(x,y) | x <- [1,2,3], y <- [4,5]]

-- Dependent generators
[(x,y) | x <- [1..3], y <- [x..3]]

-- Conditions
[x | x <- [1..10], even x]
```

**Enumerations, Range**

```haskell
[1..10]     
--[1,2,3,4,5,6,7,8,9,10]

[1, 3..100]
--[1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99]

['a'..'z']
--"abcdefghijklmnopqrstuvwxyz"
```

### Maps

```haskell
-- New map, values and keys must have the same type.
m = #{"foo" => "bar", "bar" => "foo"}

-- Match Map
#{"foo" := a, "bar" := b} = m
a :: String               -- "bar" 
b :: String

-- get, put
m1 = Map.put "key" "val"
Map.get "foo" m :: String -- "bar"
Map.get "key" m1 :: String --"val"

-- keys, values
keys   = Map.keys   m :: [String]
values = Map.values m :: [String]
```

### Records

```Haskell 
-- declare a Person record
data Person = Person {name :: String, age :: Integer}

-- or
data Person = {name :: String, age :: Integer}

-- create a Person record
p = Person {name = "John", age = 12}

-- update a Person record
p1 = p {name = "Miles", age = 20}

-- getters
name = p1.name :: String
age = p1.age   :: Integer
```

### Ports

Erlang port identifier identifies an Erlang port.

### PIDs

Erlang process identifier, pid, identifies a process.

## User-defined Types

```Haskell
-- type synonym
type Name = String
"Miles" :: Name
"Miles" :: String

newtype CInt = CInt Integer
1 :: Integer
CInt 1 :: CInt

-- sum datatype
data Color = Red | Green | Blue
Blue :: Color

-- product datatype
data Pair = Pair Integer Integer
Pair 3 4 :: Pair

-- record product datatype
data Person = Person {
  name :: String
  age :: Integer
  address :: String
}
Person {name = "Miles", age = 50, address = "NY"} :: Person

-- generic datatype (maybe for example)
data Maybe a = Just a | None
data Result val err = Ok Val | Error err

-- recursive datatype
data Tree = Leaf Integer | Node Tree Tree
```

## Functions

A function is a mapping from values of one type to values of another type.

### Function Definition and Application

*Currying*

```haskell
-- uncurried
plus :: (Integer, Integer) -> Integer
plus (x, y) = x + y

-- sum is the curried version of plus
sum :: Integer -> Integer -> Integer
sum x y = x + y
```

*Partial application*

```haskell
sum 1  2      :: Integer
sum 1 (2 + 3) :: Integer 

add2 = sum 1  :: Integer -> Integer -- partially applied
x    = add2 3 :: Integer        -- x = 5
```

### Polymorphic Functions

```haskell
length :: forall a. [a] -> Integer

-- example
nats25 :: [Integer]
nats25 = [0..25]

letters :: [Char]
letters = ['a'..'z']

n1 = length nats25   -- 26
n2 == length letters -- 26

zip :: forall a b. [a] -> [b] -> [(a,b)]
ordL = zip nats letters 

-- [(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e'),(5,'f'),(6,'g'),(7,'h'),(8,'i'),(9,'j'),(10,'k'),(11,'l'),(12,'m'),(13,'n'),(14,'o'),(15,'p'),(16,'q'),(17,'r'),(18,'s'),(19,'t'),(20,'u'),(21,'v'),(22,'w'),(23,'x'),(24,'y'),(25,'z')]
```

### High-Order Functions

```haskell
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x
```

### Recursive Function

```haskell
fact n = if n == 0 then 1 else n * fact (n - 1)

length []       = 0
length (x : xs) = length xs + 1
```

### Lambda (Anonymous Function)

```Haskell
multBy :: Integer -> Integer -> Integer
multBy n = \m -> m * n

mean :: Integer -> Integer -> Integer
mean = \x y -> (x + y) `div` 2  -- f = (\x -> \y -> (x + y) `div` 2)
```



## Pattern Matching

### Pattern Matching

```haskell
(x, y) = (1, 2)

-- function declartion via pattern matching
allEmpty [] = True
allEmpty _ = False

-- pattern matching stops when it finds the first match
```

### Guarded Equations

```haskell
abs n | n > 0     = n
      | otherwise = -n
```

### case .. of

```haskell
rgb = Red

f = case rgb of
      Red   -> "red"
      Green -> "green"
      Blue  -> "blue"
-- f has value "red"

g = case rgb of Green -> "red"; _ -> "not red"
-- g has valur "not red"

```

### if .. then .. else

```haskell
-- Every `then` must have a corresponding `else`
abs x = if x > 0 then x else -x

-- Indentations
sign x = 
  if x > 0
    then print "pos"
    else if x < 0
      then print "neg"
      else print "zero"
```



### Statement terminator

*TODO: next line has equal or less indentation, or* ;

### Blocks

TODO: begin *expr* ; *…* end

## Operators

() ?

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

### Logical Operators/Functions

| Operator | Name |
| -------- | ---- |
| &&       | And  |
| \|\|     | Or   |
| not      | Not  |
|          |      |

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

### Module Declaration and Export

The name of a module must start with a capital letter.

```haskell
-- Declare a module and export all the types and functions
module MyMod where

-- Declare a module and export some types or functions
module MyMod (Maybe(..), add) where

data Maybe a = Just a | Nothing

add :: Integer -> Integer -> Integer
add x y = x + y
```

### Main

```haskell
-- Main
module Main (main) where

main = println "Hello World"
```

### Import

```haskell
import Data.List
import Data.Map (keys, values)

nth 1 [1..10]            -- 1
keys #{"key" => "val"}   -- ["key"]
values #{"key" => "val"} -- ["val"]

-- Qualified Imports
import Data.Set as Set
import Data.Map as Map

Map.get "foo" #{"foo" => "bar"}
```

## Libraries

### Prelude

The Prelude module is imported by default.

| Function | Example |
| -------- | ------- |
| min      | min 1 2 |
| max      | max 1 2 |
|          |         |

### List



### Math - to do

```haskell
sqrt power exp log sin cos tan asin acos atan atan2
truncate round floor ceiling
```

### Date and Time - to do



## IO - todo



## Examples

### Hello World

```haskell
module Example (add) where

add :: Integer -> Integer -> Integer
add x y = x + y

main = printLn "hello world!" ++ show add
```

## Reserved Words

```haskell
myAtom ado as case class data derive do else false forall forallu foreign hiding import if in infix infixl infixr instance kind let module newtype of then true type where 
```

## Author

- Feng Lee <feng@emqx.io>
- Yang Miao <yangm@emqx.io>
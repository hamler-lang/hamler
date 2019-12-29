
# The Hamler Programming Language

`Hamler` is a functional programming language inspired by Haskell and Stardard ML, and running on Erlang VM.

## Why Hamler?

- Functional programming
- Compile-time type checking and type inference
- Embrace the Erlang VM

## Features

- Functional Programming
- Haskell and Standard ML like
- ADT and Type inference
- Currying and Partial Application
- Pattern Match
- Guards, or conditional matches
- List Comprehension
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
2 :: Integer
1.0 :: Float
3.14 :: Double
"hello" :: String

{-- Variables --}
a = 1
let b = 2
let s = "hello"
```



```haskell


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



## Types

A type is a set of values.

### Basic Types

| Type             | Values        | Description |
| ---------------- | ------------- | ----------- |
| Bool             | True \| False |             |
| Atom ??          | 'Atom'        |             |
| Char             | ?             |             |
| Integer (Number) | 1, 2, -10     |             |
| Float            | 3.14          |             |
| Double           | ?             |             |
| String           | "hello"       |             |
| Binary           |               |             |
|                  |               |             |
|                  |               |             |



### Tuples

```haskell
{- Tuple -}
(1, "a", True) -- Tuple
(1,"a")
```

### Lists

```haskell
{- List --}
[] -- empty list
[1,2,3] -- List Integer
1 : 2 : 3 : [] -- cons
```

### Dict(Map)

###Enum, Range

```haskell
{- Enumerations, Range -}

[1..10]
[1,3..100]
['a' .. 'z']
```





### User-defined Types

### Algebraic Data Types



## Functions

A function is a mapping from input to output.

### Function Definition

### Function Application

### High-order Function

### Currying and Partial Application

### Lambda (Anonymous Function)

```erlang
fun x -> fun y -> (x + y) div 2
```



## Expressions

### let

### let .. in ..

### case .. of

### if .. then .. else

### where

### List Comprehension

```haskell
{- List Comprehensions -}

[x*2 | x <- [1,2,3]] -- return [2,4,6]
```



### Pattern Matching

### Guards

### 

## Operators

### Arithmetic Operators

`+ - * / div rem`

| Op            | Name     |
| ------------- | -------- |
| +             | Add      |
| -             | Subtract |
| *             | Multiply |
| /             | Divide   |
| div           |          |
| mod (or rem?) |          |
|               |          |
|               |          |
|               |          |



### Logical Operators

`&& || not`

| Op      | Name |
| ------- | ---- |
| &&      | And  |
| \|\|    | Or   |
| not     | Not  |
| andalso |      |
| orelse  |      |
|         |      |
|         |      |
|         |      |
|         |      |



### Relational Operators

`== /= < > <= >=`

| Op   | Name      |
| ---- | --------- |
| ==   | Equal     |
| /=   | Not Equal |
| <    | Great     |
| >    |           |
| <=   |           |
| >=   |           |
|      |           |
|      |           |
|      |           |



## Modules

### Main

### Export

### Import



## Effects

TODO: How to handle side effects Monad??

## IO

TODO...

## Examples

### Hello World

```haskell
module Example export (add) where

type Bool = True | False

let a = 1
let b = 2

add :: Int -> Int -> Int
add x y = x + y

main = printLn "hello world"
```

## Reserved Words

```haskell
case class do else export if import in let of module then type where 
```

## Author

Feng Lee <feng@emqx.io>


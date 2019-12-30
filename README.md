
# The Hamler Programming Language

`Hamler` is a functional programming language inspired by Haskell and Stardard ML, and running on Erlang VM.

## Why Hamler?

- A Functional programming language;
- Compile-time type checking and type inference;
- Running on the Erlang's VM

## Features

- Functional Programming
- Haskell and Standard ML like
- ADT and Type Checking/Inference
- Currying and Partial Application
- Pattern Matching
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

## Types

A type is a set of values.

### Basic Types

| Type             | Values        | Description  |
| ---------------- | ------------- | ------------ |
| None             | None          | null         |
| Bool             | True \| False | Boolean type |
| Atom ??          | 'Atom'        |              |
| Char ??          | ?             |              |
| Int ??           | 1, 2, -10     | Integer type |
| Integer (Number) | 1, 2, -10     |              |
| Float            | 3.14          | Float type   |
| Double           | ?             |              |
| String           | "hello"       |              |
|                  |               |              |

### Tuples

```haskell
{- Tuple -}
(1, "a", True)
(1,"a")
```

### Lists

```haskell
{- List --}
[] -- empty list
[1,2,3] -- List Integer
1 : 2 : 3 : [] -- cons
```

### Enum, Range

```haskell
{- Enumerations, Range -}

[1..10]
[1,3..100]
['a'..'z']
```

### Binaries/Bitstrings

```erlang
<<1,2,3>>
<<1:16,2:4,3:4>>
```

### Maps (Dict)

TODO: ...

### Record

```haskell
{name = "John", age = 12}
```

### User-defined Types

```haskell
-- type synonym
type Name = String
n = "Miles" :: Name

-- sum type
type Color = Red | Green | Blue
c = Blue

-- pattern match sum type
case c of
  Red -> "red"
  Green -> "green"
  Blue -> "blue"
  _ -> "unknown"

-- product type
type Pair = Pair Int Int
p = Pair 3 4

-- record product type
type Person = Person {
  name :: String
  age :: Int
  address :: String
}
person = Person {name = "Miles", age = 50, address = "NY"}

-- generic type ??
type TwosomeType a b = Twosome a b
p = Twosome ("pi", 3.14)

-- recursive type
type Tree = Leaf Int | Node Tree Tree
```

### Algebraic Data Types

```haskell
-- Maybe
type Maybe a = Just a | None
list = [Just(3), None, Just(-4)]
```



## Functions

A function is a mapping from input to output.

### Function Definition

```haskell
{- Functions -}

sum :: Int -> Int -> Int
sum a b = a + b
```

### Function Application

```haskell
sum 1 2
sum 1 (2 + 3)
```

### High-order Function

### Currying and Partial Application

### Lambda (Anonymous Function)

```erlang
fun x -> fun y -> (x + y) div 2
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
  where x = 3
        y = 2 * x
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
```

### List Comprehension

```haskell
{- List Comprehensions -}

[x*2 | x <- [1,2,3]] -- return [2,4,6]
```

### Pattern Matching

### Guards

### Statement terminator

*next line has equal or less indentation, or* ;

### Blocks

begin *expr* ; *…* end

## Operators

### Arithmetic Operators

| Operator | Name     | Example |
| -------- | -------- | ------- |
| +        | Add      |         |
| -        | Subtract |         |
| *        | Multiply |         |
| /        | Divide   |         |
| div      |          | div 7 3 |
| rem      | Remain   | rem 7 3 |
|          |          |         |

### Logical Operators

| Operator | Name     |
| -------- | -------- |
| &&, and  | And      |
| \|\|, or | Or       |
| xor      | Xor      |
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
|          |             |
|          |             |
|          |             |

### Bit Operators

| BitOp | Name            |
| ----- | --------------- |
| band  | Bit and         |
| bor   | Bit or          |
| bnot  | Bit not         |
| bxor  | Bit xor         |
| bsl   | Bit shift left  |
| bsr   | Bit shift right |
|       |                 |
|       |                 |
|       |                 |

## Modules

### Main

```haskell
-- Main
main = putStrLn "Hello World"
```

### Export

### Import

## Libraries

### Core

The library which is always imported.

| Function | Example |
| -------- | ------- |
| min      | min 1 2 |
| max      | max 1 2 |
|          |         |
|          |         |
|          |         |
|          |         |
|          |         |
|          |         |
|          |         |

### Math

```haskell
sqrt power exp log sin cos tan asin acos atan atan2
truncate round floor ceiling
```



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
and andalso case class do else export if fun import in let of module not orelse then type where
```

## Author

Feng Lee <feng@emqx.io>


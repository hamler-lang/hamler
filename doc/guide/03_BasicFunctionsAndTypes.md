

# Basic Types, Functions and Operators





## 3. 2 Operators



## 3. 3 Functions

When we define a new function, we can give it a type signature. For exmaple `double` is a function takes an `Integer` and gives an `Integer` doubled as output.

```haskell
double :: Integer -> Integer
double x = x * 2
```

**Lambda**

There is also lambda in Hamler, here is an example on how we rewrite double.

```
double' :: Integer -> Integer
double' = \x -> 2 * x
```

It become really handy when we need to make an anoumynous function.

```

```

**Curry**

```haskell
--Curry
--This is uncurried (+)
add :: (Integer, Integer) -> Integer
add (x, y) = x + y

--This is curried (+)
plus :: Integer -> Integer -> Integer
plus x y = x + y
```

**Partial Application**

```Haskell
-- plus :: Integer -> (Integer -> Integer) This is one of the example of higher order functions
>:t plus 2
plus 2:: Integer -> Integer
>let plusTwo = plus2
>plusTwo 3
5
```



## 3. 4 Quantified Types

They are also known as **polymorhphic types**.

```Haskell
>:type id
id :: forall a. a -> a
```

The key word `forall`indicates that id is univerally quantified, meaning that id can be applied with any type.

```Haskell
>id 1
1
```

A more complicated example is `flip`. `flip`is also a [high-order function](), which will be explained in the later chapter.

```Haskell
>:type flip
forall a b c. (a -> b -> c) - > b -> a -> c
```



## 3. 3 Notes On Indentations

Like all ML Language Family, Hamler is indentation sensitive. Any declaration in the same block should have same level of indentation. In the case of a declaration spans more than one line, the other lines have to be intended past the first line.

```Haskell
flip x f = f
x                   -- NOT OKAY, Hamler will see x as a seperate declaration

flip f x = f
    x               -- OKAY, but not recommended
```

**`Let` and `Where` Block**

Keywork such as Let and Where introduces new block, where further indentation is needed.

```haskell
distance x y = sqrt z
  where
    z = x' + y'
    x' = x * x
    y' = y * y
```



## 3. 4 Define your own types

**Type Synonym**

Type synonym can be used to simplify a long type name to make code more readable.

```Haskell
>:i String
type String = [Char]
```

Or you can define you own synonym name or a record.

## 3. 5 Record

```Haskell
type Name = String

type Person =
  { firstName  :: Name
  , secondName :: Name
  }

{-
This is syntax sugared
"type Person = Record (FisrtName  :: Name , SecondName ::Name)"
-}
```

Fields can be accessed by `.`

```haskell
leader :: Person
leader = {firstName : "John", sastName : "Portsman"}

>leader.fisrtName
"John"
```

This is how we update a record.

```haskell
newLeader :: Person
newLeader = Leader {fisrtName : "James"}

>newLeader.lastName
"Portsman"
```




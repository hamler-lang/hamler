# Recursions and Higher Order Functions

- [Intro](#intro)
- [Recursions on more complicated datatypes](#recursions-on-more-complicated-datatypes)
- [Map, filter and fold](#map-filter-and-fold)
- [List Comprehensions](#list-comprehensions)
- [Higher Order Functions](#higher-order-functions)

---

## Intro

Resursion is an important technique in programming, especially in functional programming.

Simple examples:

```haskell
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```



---

## Recursions on more complicated datatypes

Definition of the datatype list is recursive. So,when we define a function for such datatypes, it comes naturally to define the function recursively.

```haskell
length :: forall a . [a] -> Integer
length  []    = 0
length (x:xs) = 1 + length xs
```



---

## Map, filter and fold

`map`, `filter` and `foldr` are three commonly used functions to manipulate a list. `map` applys `f` on all `a`s in a list of `a`. `filter` just filters the list according to `p`. `foldr` destructs the list by replacing every `:` with a operator/or function.

Here are the definitions.

```haskell
map :: forall a b. (a -> b) -> [a] -> [b]
map f  []      = []
map f (x:xs) = f x : xs

filter :: forall a. (a -> Boolean) -> [a] -> [a]
filter p []     = []
filter p (x:xs) = if f x then (x : filter p xs)
                         else filter p xs

foldr :: forall a b. (a -> b -> b) -> b -> [a] -> b --simplified defination see typeclass for more info
foldr f k []     = k
foldr f k (x:xs) = f x (foldr f k xs)
```

Here are some examples on the usage.

```haskell
>map (+1) [1,2,3,4,5]
[2,3,4,5,6]

>filter (> 0) [-3,-2,-1,0,1,2,3]
[1,2,3]

>foldr (+) 0 [1,2,3,4,5]
15
```



---

## List Comprehensions

There is an alternative way to define map and filter, which is to use list comprehension.

```haskell
map f xs    = [f x | x <- xs]
filter p xs = [x | x <- xs, p x]
```

With list comprehension we can also do things like:

```haskell
> [x + y | x <- [1..2], y<- [1..3]]
[2,4,5]

-- .. is syntax sugar for range
> [1..10]
[1,2,3,4,5,6,7,8,9,10]

```



---

## Higher Order Functions

Functions like map, filter and foldr are also called higher order functions, becuase they take a function as their argument. A higher order function takes a function as its argument or/and returns a function as it's result.

Here are some more examples of such functions.

```haskell
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

compose :: forall a b. (b -> c) -> (a -> b) -> a -> c
compose g f x = g (f x)
```

# More Types and Pattern Matching



## 4. 1 Algebraic Data types

Using algebraic data types we are saying some datatype can be one of the many things, distingushed by and identifies by what is called a constructor.

For example, `Maybe a` is saying that If something has type Maybe a, it can either be a value which has an `a ` type and wrapped by a constructor `Just` or an empty value `Nothing`

```haskell
data Maybe a = Just a 
             | Nothing
```

Another example is `List`, from its defination we can see that it has a recursive structure. So we can have whatever number of elements in our list, but they have to have the same type.

```haskell
data List a = Cons a (List a)
            | Empty
{-
data [a] = a : [a] 
         | []
-}
```



## 4. 2 Map

Map is the Map from Erlang. `Map k v` is the type of a Map.

We can construct a Map like this:

```Haskell
m1 :: Map String Integer
m1 = #{"Hello" => 5, "World" => 17}  

> lookup m1 "Hello"
Just  5

> insert "!" 0 m1 
#{"Hello" => 5, "World" => 17, "!" => 0}

```



## 4. 3 Newtypes

Newtypes is used to distinguish two types which have have the same type of value but different units/meanings.

For example:

```haskell
newtype Email = Email String

m1 :: Map Email Integer 
m1 = empty
--This is forces we can only pass a String with a contrutor Email.
--So insert "abc" 123 m1 will fail
```



## 4. 4 Simple Pattern Matching



```haskell
fib 0 = 1
fib 1 = 1 
fib x = fib (x - 1) + fib (x - 2)
```



## 4. 5 Guards

```haskell
max x y | x > y     = x 
        | otherwise = y
```



## 4. 6 List Patterns

```Haskell
isEmpty :: forall a.[a] -> Boolean
isEmpty  []      = true
isEmpty (x : xs) = false 
```



## 4. 7 Record Patterns



```haskell
showPerson :: { firstName :: Name, lastName :: Name } -> Name
showPerson { firstName: x, lastName: y } = y <> ", " <> x

> showPerson { firstName: "Phil", lastName: "Freeman" }
"Freeman, Phil"

> showPerson { firstName: "Phil", lastName: "Freeman", location: "Los Angeles" }
"Freeman, Phil"
```



## 4. 8 Map Patterns

We can also pattern match on `Map`s, and this is very similar to `Record`s, except some syntax changes. For example, `getID` let us to get the ID of Wang from a map where we have to have at least Wang, Thomas and Leeming as keys. 

```haskell
getID :: Map String Integer -> Maybe Integer
getID #{ "Wang":= x, "Thomas" := y, "Leeming" := z } = Just x
getID _                                              = Nothing

```



## 4. 9 Binary Patterns

Matching on binaries is just like how it is done in Erlang. Int the following example, we are trying to get a 24 bit integer out of the Binary  passed to getA.

```haskell
getA :: Binary -> Just Integer
getA << (a):24:Big-Integer | (b):4:Binary-Little | (c):32:Binary >> = Just a
getA _                                                               = Nothing
```

`Big` and `Little` meas the endianess in the part we need. `Integer` or `Binary` are the type we will give to after we extract the segment. The number of bits of the segment depends on the size of segment we need and the type we assign. If they type we assign is an `Integer` then the get same number of the size, which is required to be evenly divisible by 8. If it is a `Binary` we want, it will need 8 times the size of bits. 



## 4 . 10 Case Expressions

With `case` we can also pattern match on the value after some computations, when there is no need to bind the intermediate result.

```haskell
plus (x, y) = x + y

sumUpTo :: Integer -> (Integer, Integer) -> Boolean
sumUpTo x p = case plus p of
                x -> true
                _ -> false
```

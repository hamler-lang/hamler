

# Type Class

## 6. 1 Intro

A type class defines some types related by their operations. This is saying that typeclasses are usuaslly defined in terms of those operations. 

For example, we can put all types that can be converted to a `String` in the same type family called `Show` . 

We can introduce this `Show` type class by:

```Haskell
class Show a where 
  show :: a -> String
```

Then we gave an instance to the type class.

```haskell
instance Show String where
  show s = s

instance Show Boolean where
  show true = "true"
  show false = "false"
```



## 6. 2 Functor

We have seen how map is defined for `[]`, and we can also map on other types. We call these types Functor if they can be `map`ped and stratify the Functor law at the same time.

 Functor laws:                                   `map id = id     |       map (compose g f)  = map g . map f `

```haskell
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
  
instance Functor Maybe where
  map f (Just x) = Just (fn x)
  map f  Nothing = Nothing
 
instance Functor [] where
  map f []     = []
  map f (x:xs) = f x : map f xs
```



## 6. 3 Other common type classes

```haskell
class Eq a where
  eq :: a -> a -> Boolean

-- data Ordering = LT | GT | EQ
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  
class Foldable f where
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b

class Semigroup a where
  append :: a -> a -> a
```




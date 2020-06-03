# Applicative and Monad

In this chapter we are going to look at two very important type class, Applicative Functor and Monad.

**Applicative**

Let's kick off with how the Applicative class is defined. From the defination we can see that Applicatives they are Functors with two more operations, which is pure and apply. `pure` wraps some value to make an applicative functor. `apply` is a bit more complicated.

Let's just look at its type, does that ring a bell? Yeah, it looks like map except we have functions wrapped in side an applicative functor. What `apply` does is extract the function(s) from the functor and map them to the `f a` .

```haskell
class Functor f => Applicative f where
  pure  :: forall a. a -> f a
  apply :: forall a b. f (a -> b) -> f a -> f b

infixl 4 apply as <*>
```

Here are some examples of intances of `Applicatve`

```haskell
instance Applicative Maybe where
  pure = Just
  Nothig   <*> mb = Nothing
  (Just f) <*> mb = map f mb

instance Applicative [] where
  pure = (:[])
  fs <*> xs = [f x | x <- xs, f <- fs]
```

Let's have a closer look at instance `Applicative []` , we can see that every `f` in the list will get applied to all the elements in the list.  So with  `(+) <$> [1,2] <*> [3,4,5]`, we will have a non-deterministic computation on `(+)`.

**Monad**



```haskell
class Applicative m => Monad m where
  bind :: forall a b. m a -> (a -> m b) -> m b
  return :: forall a. a -> m a
```



**`lift`ing**

```Haskell
liftA1 :: forall a b f. Applicative f => (a -> b) -> f a -> f b
liftA1 f a = pure f <*> a

liftA2 :: forall a b c f. Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = pure f <*> a <*> b

liftM1 :: forall a b m. Monad m => (a -> b) -> m a -> m b
liftM1 f ma = do
  a <- ma
  return (f a)

liftM2 :: forall a b c m. Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = do
  a <- ma
  b <- mb
  return (f a b)
```


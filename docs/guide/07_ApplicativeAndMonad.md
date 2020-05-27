

# Applicative and Monad



**Overview**

```haskell
class Functor f => Applicative f where
  pure  :: forall a. a -> f a
  apply :: forall a b. f (a -> b) -> f a -> f b
  
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


module Tuple where

data Tuple a b = Tuple a b


fst :: forall a b. Tuple a b -> a
fst (Tuple a _) = a

snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b

swap :: forall a b. Tuple a b -> Tuple b a
swap (Tuple a b) = Tuple b a



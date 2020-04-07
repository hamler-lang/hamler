module Example where


-- function with a single argument
identity :: forall a. a -> a
identity = \ x -> x

-- function with multiple arguments
constant :: forall a b. a -> b -> a
constant x y = x

-- function application
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

-- Boolean literal
boolean :: Boolean
boolean = true

-- Int literal
integer :: Int
integer = 7

-- Number literal
number :: Number
number = 1.2

-- Char literal
character :: Char
character = 't'

-- String literal
string :: String
string = "thran"

-- Array literal
array :: Array Int
array = [1, 2, 3]

-- empty Record literal
record :: {}
record = {}

-- non-empty Record literal
nonEmpty :: { name :: String }
nonEmpty = { name: "thran" }

-- record access
getName :: { name :: String } -> String
getName person = person.name

-- case expression
switch :: Int -> Int
switch x = case x, x of
  0, 0 -> 0
  1, z -> z
  y, 1 -> y
  _, _ -> x

-- conditional expression
not :: Boolean -> Boolean
not x = if x
  then false
  else true

-- "let ... in ..." expression
letIdentity :: forall a. a -> a
letIdentity = let
  f = identity
  g = identity
  in g f

-- "... where ..." expression
whereIdentity :: forall a. a -> a
whereIdentity = g f where
  g = identity
  f = identity

-- newtype
newtype Tagged tag value = Tagged value

-- type class
class Semigroup a where
  append :: a -> a -> a

-- super class
class Semigroup a <= Monoid a where
  empty :: a

-- type class instance
instance semigroupInt :: Semigroup Int where
  append _ _ = 0

-- operators are not present in corefn
--infix 5 append as +

-- using a type class
--triple :: forall a. Semigroup a => a -> a
--triple x = x + x + x

-- partial function
--partial :: Partial => Int -> Int
--partial 0 = 0

-- named pattern
named :: forall a. a -> a
named x = case x of
  y@_ -> y

-- mutually recursive declarations
mutualA :: forall a b. a -> b
mutualA x = mutualB x

mutualB :: forall a b. a -> b
mutualB x = mutualA x

-- data without constructors are not present in corefn
data Void

-- data, one constructor
data UnitT = UnitC

-- using a constructor
unit :: UnitT
unit = UnitC

-- data, multiple constructors
data Toggle = Off | On

-- data with arguments
data T a = C a

-- using a constructor with arguments
intT :: T Int
intT = C 0

-- constructor with multiple arguments
data Tuple a b = Tuple a b

-- using a constructor with multiple arguments
tuple :: forall a b. a -> b -> Tuple a b
tuple x y = Tuple x y

-- adt with an argument
data Maybe a = Nothing | Just a

-- using an adt with an argument
just :: forall a. a -> Maybe a
just x = Just x

-- adt with multiple arguments
data Either a b = Left a | Right b

-- using an adt with multiple arguments
right :: forall a b. b -> Either a b
right x = Right x

-- recursive adt
data List a = Nil | Cons a (List a)

-- type operators are not present in the corefn
infixr 6 Cons as :

-- using a recursive adt
numbers :: List Int
numbers = 1 : 2 : Nil

-- negative numbers
negativeOne :: Int
negativeOne = -1

-- dummy `negate` definition to avoid pulling in the prelude
negate :: forall a. a -> a
negate x = x

-- do notation
perform :: forall a b. a -> b -> b
perform effect query = do
  effect
  result <- query
  effect
  _ <- query
  result

-- dummy `bind` definition to avoid pulling in the prelude
bind :: forall a b. a -> (a -> b) -> b
bind x f = f x

-- dummy `discard` definition to avoid pulling in the prelude
discard :: forall a b. a -> (a -> b) -> b
discard = bind

module Chapter6 where

-- 6.1/6.2 type classes
-- types and type classes are in some ways opposites
-- type declaration defines how a type is created
-- type class declaration defines how a set of types is consumed (like interfaces)
data Us = Julien | Ruth deriving Show
instance Eq Us where
  (==) Julien Julien = True
  (==) Ruth Ruth     = True
  (==) _ _           = False

greet :: Us -> String
greet Julien = "Hello"
greet Ruth   = "RUUUUUUTHHH"
-- :set -Wall
-- partial Julien = "partial"
-- this warns

-- exercises - white the Eq instances
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = x == x' && y == y'

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _                      = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x')     = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _ _                      = False

--
data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show)

--
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk a2b a b = (a2b a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
-- arith a2b i a = (+) i (a2b a)

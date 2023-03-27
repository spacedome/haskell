-- |

module Chapter7 where

-- case syntax
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

nums' x =
  case y of
    LT -> -1
    GT -> 1
    EQ -> 0
  where y = compare x 0

-- guards
myAbs :: Integer -> Integer
myAbs x
  | x < 0       = (-x)
  | otherwise   = x

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2    = "RIGHT"
  | otherwise           = "NOT RIGHT"

-- composition (.)
comp = negate . sum $ [1, 2, 3, 4, 5]

module Chapter4 where

import Data.Tuple
-- 4.3 Data(type) Declaration
data Mood = Blah | Woot deriving Show
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
-- 4.4 Numeric Types
z = 1 :: Word -- Word is for non-negative integers
max_int = maxBound :: Int
wrapped = max_int + 1 :: Int -- negative
-- 4.5 Comparison
x1 = (4 /= 5) -- Eq type class
x2 = ("abc" < "bbc") -- Ord type class
-- 4.6 Bool
false = (not True) && (True || False)
x3 = if False then "1" else "2"
greet :: String -> IO ()
greet name =
  if ruthy name
     then putStrLn "RUUUUUUUUTH"
  else
    putStrLn ("Hello " ++ name)
  where ruthy w =
          w == "Ruth"
-- 4.7 Tuple - a PRODUCT type
-- data (,) a b = (,) a b
tup = (,) 1 2 -- can also just type (1, 2)
x4 = fst tup -- get first value - must be 2-tuple
x5 = snd tup -- get second value - muct be 2-tuple
-- need to import Data.Tuple for this
swapped = swap tup
-- can define these ourselves
swap' :: (a, b) -> (b, a)
swap' (a, b) = (b, a)
fst' :: (a, b) -> a
fst' (a, b) = a
-- 4.8 Lists

-- 4.9 Exercises
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome s = s == reverse s

myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else -x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

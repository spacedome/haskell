-- |

module Chapter8 where

fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact(n - 1)

-- go function allows where to accept more arguments than top level
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

-- 8.6 exercises
-- 1. d - [[Bool]]
-- 2. b
-- 3. a, b, c, d
-- 4. b

cattyConny :: String -> String -> String
frappe :: String -> String
appedCatty :: String -> String
cattyConny x y = x ++ " mrow " ++ y
flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"

-- Currying
-- 1. "woops mrw woohoo!"
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. "woops mrow lue mrow haha"
-- 5. "pink mrow haha mrow green mrow woops mrow blue"
-- 6. "are mrow Pugs mrow awesome"
-- Recursion
-- 2.
sumAll :: (Eq a, Num a) => a -> a
sumAll num = go num 0
  where go n s
         | n == 0    = s
         | otherwise = go (n-1) (s + n)
-- 3.
multRecursive :: Integral a => a -> a -> a
multRecursive x y = go x y 0
  where go x y s
         | x == 0 = s
         | x > 0  = go (x - 1) y (s + y)
         | x < 0  = go (x + 1) y (s - y)

mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91(mc91 (n + 11))

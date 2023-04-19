-- |

module Lists2 where

import Data.List



-- Problem 11
-- modified run length encoding
data RLE a = Multiple a Int | Single a deriving Show
encodeModified :: Eq a => [a] -> [RLE a]
encodeModified = map encodeInner . group
  where
    encodeInner (x:[]) = Single x
    encodeInner (x:xs) = Multiple x (succ $ length xs)

--Problem 12
-- decode the previous
decodeModified :: [RLE a] -> [a]
decodeModified = concat . map decodeInner
  where
    decodeInner (Single x) = x:[]
    decodeInner (Multiple x n) = take n $ repeat x

-- Problem 13
-- this seems annoying

-- Problem 14
-- duplicate elements of a list
dupli :: [a] -> [a]
dupli = concat . map (replicate 2)
-- from solution
dupli' :: [a] -> [a]
dupli' = concatMap (replicate 2)
dupliM xs = xs >>= (\x -> [x,x])


-- Problem 15
-- repicate elements of a list
repli :: [a] -> Int -> [a]
repli xs n = concat $ map (replicate n) xs
-- from solutions
repliM xs n = xs >>= replicate n

-- Problem 16
-- drop every n-th element of a list
-- dropEvery :: [a] -> [a]

-- Problem 17
-- Split into two parts
split :: [a] -> Int -> ([a], [a])
split x n = (take n x, drop n x)
-- from solutions
split' = flip splitAt

-- Problem 18
-- take a slice
slice :: [a] -> Int -> Int -> [a]
slice x a b = take (b-1) $ drop (a-1) x

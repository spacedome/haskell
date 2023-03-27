-- |

module Chapter10 where

-- 10.10 exercises
stops = "pbtdkg"
vowels = "aeiou"
triples a b = [(x, y, z) | x <- a, y <- b, z <- a]
pWords = filter (\(x,y,z) -> x=='p') $ triples stops vowels

avgWordLen x =
  (/) (sum (map (fromIntegral . length) (words x)))
      (fromIntegral $ length (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (||) False . map f

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (||) False . map (==e)
myElem' e = myAny (==e)

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
-- myMap f = foldr (\x xs -> (f x):xs) []
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then x:xs else xs) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (++) [] . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip ffoldr1 (\a b -> if f a b == GT then a else b))

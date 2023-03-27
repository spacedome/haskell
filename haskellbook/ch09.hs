-- |

module Chapter9 where


-- lists - data [] a = [] | a : [a]
-- pattern matching
myHead (x : _) = x
myTail (_ : xs) = xs
-- this fails on [] - use Maybe
-- data Maybe a = Nothing | Just a
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

-- exercises
-- 1.
myWords :: String -> [String]
myWords s = go s []
  where go s list
         | s == ""   = reverse list
         | otherwise = go (dropWhile (==' ') $ dropWhile (/=' ') s) (takeWhile (/=' ') s : list)
-- 2.
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
myLines :: String -> [String]
myLines s = go s []
  where go s list
         | s == ""   = reverse list
         | otherwise = go (dropWhile (=='\n') $ dropWhile (/='\n') s) (takeWhile (/='\n') s : list)
-- 3.
split :: String -> Char -> [String]
split s c = go s []
  where go s list
         | s == ""   = reverse list
         | otherwise = go (dropWhile (==c) $ dropWhile (/=c) s) (takeWhile (/=c) s : list)

-- list comprehensions
mySqr  = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
myTuples = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
numberSqrAndCube = length myTuples

-- zipping
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a,b)]
zip'' l1 l2 = zipWith' (\x y -> (x, y)) l1 l2

-- exercises
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)= x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) = x == y || myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (==x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

maxBy :: (a -> a -> Ordering) -> [a] -> a
maxBy f (x:[]) = x
maxBy f (x:y:xs)
  | f x y == LT = maxBy f (y:xs)
  | f x y == EQ = maxBy f (y:xs)
  | f x y == GT = maxBy f (x:xs)

minBy :: (a -> a -> Ordering) -> [a] -> a
minBy f (x:[]) = x
minBy f (x:y:xs)
  | f x y == LT = minBy f (x:xs)
  | f x y == EQ = minBy f (x:xs)
  | f x y == GT = minBy f (y:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = maxBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = minBy compare

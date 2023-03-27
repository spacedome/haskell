-- |

module Lists where

-- Problem 1
-- find the last element of a list
myLast :: [a] -> Maybe a
myLast []     = Nothing
myLast (x:[]) = Just  x
myLast (x:xs) = myLast xs
myLast' = head . reverse
myLast'' :: [a] -> a
myLast'' = foldl (flip const) undefined

-- Problem 2
-- find the second to last element of a list
myButLast :: [a] -> Maybe a
myButLast (x:y:[]) = Just x
myButLast (x:xs)   = myButLast xs
myButLast _        = Nothing
myButLast' = head . drop 1. reverse

-- Problem 3
-- Find the k-th element of a list
elementAt :: Integral n => [a] -> n -> Maybe a
elementAt (x:_) 1 = Just x
elementAt (_:xs) n = elementAt xs (n - 1)
elementAt [] _     = Nothing
elementAt' = (head .) . flip (drop . subtract 1)

-- Problem 4
-- find the number of elements in a list
myLength :: [a] -> Integer
myLength = foldr (const (1+)) 0

-- Problem 5
-- reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
myReverse' :: [a] -> [a]
myReverse' = foldr (\x xs -> xs ++ [x]) []
myReverse'' :: [a] -> [a]
myReverse'' = foldr (flip (++) . return) ([])

-- Problem 6
-- is a list a palidrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = reverse x == x

-- Problem 7
-- Flatten an arbitrarily nested list
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List a) = foldr (++) [] $ map flatten a

-- Problem 8
-- eliminate consecutive duplicates
compress :: Eq a => [a] -> [a]
compress [] = []
compress xs = (head $ takeWhile (==(head xs)) xs):(compress (dropWhile (==(head xs)) xs))
compress' []     = []
compress' (x:xs) = x : (compress $ dropWhile (== x) xs)

-- Problem 9
-- pack duplicates into sublists
pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x : takeWhile (==x) xs ) : (pack $ dropWhile (==x) xs)

-- Problem 10
-- Run length encoding using previous problem
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\a -> (length a, head a)) . pack

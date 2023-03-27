{-# LANGUAGE NoMonomorphismRestriction #-}

module Chapter5 where

-- function types - the arrow -> is the type constructor for functions
-- /functions are values/ and -> is an infix operator
-- data (->) a b

-- 5.6 type inference
-- the compiler can infer types from functions
ourId x = x
-- ourId :: a -> a

-- 5.7 Asserting types with declarations
triple x = x * 3 -- Num a => a -> a
triple' x = x * 3 :: Integer -- constrain to Integer
-- usually do the separate type declaration before the function declaration

example = 1 -- has type (Num a => a) because of file header (instead of Integer)

-- exercises : write the function for the type signature
i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r list = reverse list

co :: (b -> c) -> (a -> b) -> a -> c
co b2c a2b a = b2c (a2b a)

a :: (a -> c) -> a -> a
a a2c a = a

a' :: (a -> b) -> a -> b
a' a2b a = a2b a

-- "type-kwan-do"
f :: Int -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int -> Char
h x = g (f x)
--
data A
data B
data C
q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e x = w (q x)
--
data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

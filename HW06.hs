{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE BangPatterns #-}
module HW06 where

import Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a s)= a:streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a s)= Cons (f a) $ fmap f s

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a $ sRepeat a

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons (f a) $ sIterate f (f a)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a s) b = Cons a (sInterleave b s)

sTake :: Int -> Stream a -> [a]
sTake n  = take n . streamToList 

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) (-1)

ruler :: Stream Integer
ruler = foldr1 sInterleave $ map sRepeat [0..]

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate f 
    where
        f :: Int -> Int 
        f a =(1103515245 * a + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs = foldl' f (Just(0,0)) xs
    where
        f (Just (!m,!n)) x = Just (if m<x then m else x, if n<x then x else m )
        f Nothing _ = Nothing
main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------
data Matrix = Matrix {get11::Integer,get12::Integer,get21::Integer,get22::Integer} deriving Show

instance Num Matrix where
   (Matrix !a1 !b1 !c1 !d1) * (Matrix !a2 !b2 !c2 !d2) = Matrix (a1*a2 + b1*c2) (a1*b2 + b1*d2) (c1*a2 + d1*c2) (c1*b2 + d1*d2) 

powerMatrix::Int->Matrix->Matrix
powerMatrix 0 _ = Matrix 1 0 0 1
powerMatrix n m
    | even n = let a = powerMatrix (div n 2) m in a*a
    | otherwise = let a = powerMatrix (div (n-1) 2) m in m*a*a

fastFib :: Int -> Integer
fastFib n = get12 $ powerMatrix n (Matrix 1 1 1 0)

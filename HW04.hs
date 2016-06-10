{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P m == P y 
      | a == b = m == y
      | a > b = y == take b m && all (==0) ( drop b m)
      | a < b = take a y == m && all (==0) ( drop a y)
        where a = length m
              b = length y
    P _ == P _ = False

 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = ""
    show (P m) 
      | all (==0) m = "0"
      | otherwise = foldl go  "" $ zip m  [0..(length m -1)]
        where go c (y,z)  
                | y == 0 =  c 
                | c == "" = showOnly y z
                | otherwise = showOnly y z ++ " + " ++ c   
              showOnly 0 _= ""
              showOnly e 0 = show e 
              showOnly (-1) 1 = "-x"
              showOnly 1 1 = "x"
              showOnly e 1 = show e ++ "x" 
              showOnly (-1) e = "-x^"++ show e  
              showOnly 1 e = "x^"++ show e  
              showOnly e f = show e ++ "x^" ++ show f 

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) b = b
plus a (P []) = a
plus (P (a:as)) (P (b:bs)) = P ((a+b): getPoly (plus (P as) (P bs)))
  where getPoly (P c) = c 

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b)= foldr plus (P [0]) (zipWith go a [0..])
    where go m n = P (replicate n 0 ++ map (*m) b)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate  (P a)    = P (map negate a)
    fromInteger a = P [fromInteger a]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P a) b = sum $ zipWith go a [0..]
    where
        go m n = m*b^n

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n a= iterate deriv a !! n

-- Exercise 9 -----------------------------------------

instance (Num a,Enum a) => Differentiable (Poly a) where
    deriv (P [])= P []
    deriv (P [_]) = P [0] 
    deriv (P a) = P (zipWith (*) (drop 1 a) [1..])
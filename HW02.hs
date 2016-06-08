{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------
count :: Eq a => a->[a]->Int
count t = length . filter (==t) 

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = count True $ zipWith (==) a b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = map  (countIn c) colors 
    where countIn =  flip count

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum $ zipWith min x y
    where 
        x = countColors a
        y = countColors b 

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove a b = Move b em (m-em) 
    where
        em = exactMatches a b
        m = matches a b


-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code = move == getMove code guess
    where 
        (Move guess _ _) = move 
-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes  = filter . isConsistent  

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n = foldl (\x _-> oneMore x) [] [1..n]
    where 
       oneMore [] = map (:[]) colors 
       oneMore cs = concatMap  (\c -> map (c:) cs) colors :: [Code]


-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = doMove $ allCodes $ length secret
    where 
        doMove :: [Code] ->[Move]
        doMove (g:gs)
            | g == secret = [move] 
            | otherwise   = move : doMove (filterCodes move gs)
            where move = getMove secret g
        doMove _ = []
-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
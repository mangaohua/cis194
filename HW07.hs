{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM = fmap

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV a b v = liftM2 swap e1 e2
  where 
    e1 = v!?a
    e2 = v!?b
    swap x y = V.update v (V.fromList [(a,y),(b,x)])

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = foldr cons_f (pure []) 
  where cons_f x ys = (:) <$> f x <*> ys 

getElts :: [Int] -> Vector a -> Maybe [a]
getElts l v = mapM g l
  where g i = v!?i

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (v!?) <$> getRandomR (0,length v-1) 


-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.replicateM n getRandom 

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n = V.replicateM n . getRandomR 

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = shuffle' (V.length vec - 1) vec
  where shuffle' n v | n==0 = return v
                     | otherwise = swapOne n v >>= shuffle' (n-1)
        swapOne i v =  swap v i <$> getRandomR (0,i)
        swap v a b = let x=v!a;y=v!b in V.update v (V.fromList [(a,y),(b,x)])

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v index = (V.filter (<pivot) v , pivot,V.ifilter abvPivot v)
  where pivot = v ! index
        abvPivot i a = i/=index && a >= pivot

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v | V.length v == 0 = V.empty
        | otherwise = 
            qsort (do y<- xs; guard (y<x); return y)
            <> V.singleton x
            <> qsort (do y<- xs;guard (y>=x);return y)
          where x = V.head v; xs = V.tail v

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v | V.length v == 0 = return V.empty
         | otherwise =
            do i <- getRandomR (0, V.length v-1)
               let (left,pivot,right) = partitionAt v i
               l <- qsortR left
               r <- qsortR right
               return (l <> V.singleton pivot <> r)
          

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select index vec | index <0 || index >= n = return Nothing
                 | otherwise = do
                   i <- getRandomR (0,n-1)
                   let (left,pivot,right) = partitionAt vec i 
                   if V.length left > index then select index left
                   else if V.length left == index then return (Just pivot)
                   else select (index-V.length left-1)  right
                   where n = V.length vec

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = do s <- suits
              l <- labels
              return (Card l s)


newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard v | V.length v == 0 = Nothing
           | otherwise = Just (V.head v, V.tail v)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck | n > V.length deck = Nothing
                | n == 0 = Just ([],deck)
                | otherwise = do 
                  (c,d) <- nextCard deck 
                  (x,y) <-  getCards (n-1) d
                  return (c:x,y)
-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100

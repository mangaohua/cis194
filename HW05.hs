{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import Parser
import Data.Bits (xor)
import System.FilePath (splitExtension)
import Data.List (sortBy)

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret o t = do
  ob <- BS.readFile o
  tb <- BS.readFile t 
  return $ BS.pack $ filter (/= 0) $ BS.zipWith xor ob tb


-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey k path = do
  c <- BS.readFile path
  let enc = BS.pack $ BS.zipWith xor (BS.concat (repeat k)) c
  BS.writeFile (fst $ splitExtension  path) enc

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
  c <- BS.readFile path
  return $ decode c 

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs v t = do
  victims <- parseFile v
  transactions <- parseFile t
  let
    match :: Maybe [TId]->Maybe [Transaction] -> IO (Maybe [Transaction])
    match (Just x) (Just y) = return $ Just $ filter (go x) y
    match _ _ = return Nothing
  match victims transactions  
  where 
    go m n= any (\z->tid n== z ) m 

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = foldl update Map.empty ts
  where update m t = 
          let receiveMoney = Map.insertWith (+) (to t) (amount t) m 
          in Map.insertWith (+) (from t) (-(amount t)) receiveMoney

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ Map.foldlWithKey compare ("",0) m
  where compare (name, amount) name' amount'  =
          if amount'>amount 
          then (name',amount')
          else (name,amount) 

-- Exercise 7 -----------------------------------------

filterFlow ::Map String Integer -> Map String Integer
filterFlow = Map.filter (/=0)

compose :: [a->a]->a ->a 
compose fs v = foldl (flip (.)) id fs $ v 

updateTrans :: Transaction->Map String Integer->Map String Integer 
updateTrans t = let f incMoney origMoney = 
                     if incMoney + origMoney==0 then Nothing
                     else Just (origMoney + incMoney)
                in Map.update (f (amount t)) (to t)  .
                    Map.update (f (negate $ amount t)) (from t)
createTransactionFromPayerPayeeTId triple = Transaction (fst payer) (fst payee) payment tid
  where 
    (payer,payee,tid ) = triple
    payment = min (snd payer) ((abs.snd) payee)

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flow tIds = 
  if null flow then []
  else transactions ++ undoTs updatedFlow updatedTId
  where 
    flowList = Map.toList flow
    payers = sortBy (\x y-> compare (snd y) (snd x) ) $ filter ((>0) . snd) $ Map.toList flow
    payees = sortBy (\x y-> compare (snd x) (snd y) ) $ filter ((<0) . snd) $ Map.toList flow
    transactions = map createTransactionFromPayerPayeeTId (zip3 payers payees tIds)
    updatedTId = drop (length transactions) tIds
    updatedFlow = filterFlow $ compose (map updateTrans transactions) flow

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON f = BS.writeFile f . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim


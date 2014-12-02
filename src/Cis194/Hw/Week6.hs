{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Week6 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector as V
import Data.List as L

-- ex1 : map "Y"/"N" to True/False

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array a) = Array (fmap ynToBool a)
ynToBool (Object o) = Object (fmap ynToBool o)
ynToBool v = v

-- ex2

parseData :: B.ByteString -> Either String Value
parseData s = fmap ynToBool (eitherDecode s)

-- ex3

data Market = Market { marketname :: T.Text
                     , x :: Int
                     , y :: Int
                     , state :: T.Text }
              deriving (Show, Generic)

instance FromJSON Market

mapResult :: (a -> Result b) -> [a] -> [b]
mapResult _ [] = []
mapResult f (a:as) =
  let bs = mapResult f as in
  case f a of
    Error _ -> bs
    Success b  -> b:bs

arrayToMarkets :: Value -> [Market]
arrayToMarkets (Array a) = mapResult fromJSON (V.toList a)

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets s = fmap arrayToMarkets (parseData s)

-- ex4

loadData :: IO [Market]
loadData = do
  fileData <- B.readFile "markets.json"
  let j = parseMarkets fileData
  case j of
    Left s -> fail s
    Right m -> return m

-- ex5

data OrdList a = OrdList { getOrdList :: [a] }
               deriving (Eq, Show)

merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge a@(x:xs) b@(y:ys) = if x < y
                          then x : merge xs b
                          else y : merge a ys

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend as bs = OrdList $ merge (getOrdList as) (getOrdList bs)

-- ex6

type Searcher m = T.Text -> [Market] -> m

marketHasName :: T.Text -> Market -> Bool
marketHasName t (Market { marketname = name }) =
  t `T.isInfixOf` name

search :: Monoid m => (Market -> m) -> Searcher m
search f t ms = L.foldl' mappend mempty (L.map f ms')
  where ms' = L.filter (marketHasName t) ms

-- ex7

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 getFirst (search (First . Just))

-- ex8

lastFound :: Searcher (Maybe Market)
lastFound = compose2 getLast (search (Last . Just))

-- ex9

allFound :: Searcher [Market]
allFound = search (:[])

-- ex10

numberFound :: Searcher Int
numberFound = compose2 getSum (search (const $ Sum 1))

-- ex11

data OrdMarket = OrdMarket { getOrdMarket :: Market }
               deriving (Show)

instance Eq OrdMarket where
  a == b = (y . getOrdMarket) a == (y . getOrdMarket) b

instance Ord OrdMarket where
  a <= b = (y . getOrdMarket) a >= (y . getOrdMarket) b

orderedNtoS :: Searcher [Market]
orderedNtoS = compose2 (L.map getOrdMarket . getOrdList)
              (search (OrdList . (:[]) . OrdMarket))

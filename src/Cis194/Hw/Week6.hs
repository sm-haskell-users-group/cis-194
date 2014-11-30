{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Cis194.Hw.Week6 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Maybe as M
import qualified Data.Text as T

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object x') = Object $ fmap ynToBool x'
ynToBool (Array x') = Array $ fmap ynToBool x'
ynToBool x' = x'

parseData :: B.ByteString -> Either String Value
parseData = (fmap ynToBool) . eitherDecode

data Market = Market { fmid :: Int
                     , marketname :: T.Text
                     , website :: T.Text
                     , state :: T.Text
                     , x :: Double
                     , y :: Double
                     }
    deriving (Show, Generic)

instance FromJSON Market

-- Is there a better way to do this?
resultToEither :: Result a -> Either String a
resultToEither (Error s) = Left s
resultToEither (Success r) = Right r

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bs = (parseData bs) >>= (resultToEither . fromJSON)

loadData :: IO [Market]
loadData = (B.readFile "data/markets.json") >>= (return . getOrFail . parseMarkets)
    where getOrFail (Right s) = s
          getOrFail (Left e) = fail e

data OrdList a = OrdList { getOrdList :: [a] }
    deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend (OrdList xs) (OrdList ys) = OrdList $ foldr insert xs ys
    where insert x' a = filter (< x') a ++ [x'] ++ filter (>= x') a

type Searcher m = T.Text -> [Market] -> m
search :: Monoid m => (Market -> m) -> Searcher m
search toM name mkts = foldr (mappend . toM) mempty $ filter ((T.isInfixOf name) . marketname) mkts

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 M.listToMaybe (search (:[]))

lastFound :: Searcher (Maybe Market)
lastFound = compose2 (M.listToMaybe . reverse) (search (:[]))

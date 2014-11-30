{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Cis194.Hw.Week6 where

import Data.Aeson
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
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

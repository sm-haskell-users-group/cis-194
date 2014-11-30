{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Cis194.Hw.Week6 where

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as B

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object x') = Object $ fmap ynToBool x'
ynToBool (Array x') = Array $ fmap ynToBool x'
ynToBool x' = x'

parseData :: B.ByteString -> Either String Value
parseData = (fmap ynToBool) . eitherDecode

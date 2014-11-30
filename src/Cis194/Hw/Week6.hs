{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Cis194.Hw.Week6 where

import Data.Aeson

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool x' = x'

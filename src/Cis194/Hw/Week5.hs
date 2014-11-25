module Cis194.Hw.Week5 where

import Ring
import Parser

data Mod5 = Mod5 Integer
  deriving (Show, Eq)

instance Ring Mod5 where
  addId                 = Mod5 0
  addInv (Mod5 x)       = Mod5 $ negate x
  mulId                 = Mod5 1
  add (Mod5 x) (Mod5 y) = Mod5 $ (mod) (x + y) 5
  mul (Mod5 x) (Mod5 y) = Mod5 $ (mod) (x * y) 5

{-instance Parsable Mod5 where-}
  {-parse s = listToMaybe . reads-}

-- parse :: String -> Maybe (a, String)

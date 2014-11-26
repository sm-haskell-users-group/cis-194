module Cis194.Hw.Week5 where

import Data.Maybe    ( listToMaybe )

import Cis194.Hw.Ring

data Mod5 = MkMod Integer
  deriving (Show, Eq)

mod5 :: Integer -> Integer
mod5 = (flip mod 5)

-- Reinventing functors! Hooray!
fromMod1 :: (Integer -> Integer) -> Mod5 -> Mod5
fromMod1 op (MkMod x) = MkMod (op x)

fromMod2 :: (Integer -> Integer -> Integer) -> Mod5 -> Mod5 -> Mod5
fromMod2 op (MkMod x) (MkMod y) = MkMod (x `op` y)

instance Ring Mod5 where
  addId = MkMod 0
  addInv = fromMod1 negate
  mulId = MkMod 1

  add = fromMod2 (\x y -> mod5 $ x + y)
  mul = fromMod2 (\x y -> mod5 $ x * y)

instance Parsable Mod5 where
  parse = (fmap (\(x, r) -> (MkMod x, r))) . listToMaybe . reads

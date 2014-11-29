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

data Mat2x2 = MkMat ((Integer, Integer),(Integer,Integer))
  deriving (Show, Eq)

matmap :: (Integer -> Integer) -> Mat2x2 -> Mat2x2
matmap op (MkMat (a, b)) = MkMat (dopart a, dopart b)
  where dopart (x,y) = (op x, op y)

matfold :: (Integer -> Integer -> Integer) -> Mat2x2 -> Mat2x2 -> Mat2x2
matfold op (MkMat (a, b)) (MkMat (x, y)) = MkMat (dopart a x, dopart b y)
  where dopart (a', b') (x', y') = (a' `op` x', b' `op` y')

instance Ring Mat2x2 where
  addId = MkMat ((0,0),(0,0))
  addInv = matmap negate
  mulId = MkMat ((1,1),(1,1))

  mul = matfold (*)
  add = matfold (+)

instance Parsable Mat2x2 where
  parse = fifth . fourth . third . second . first
    where
      first :: String -> Maybe (Integer, String)
      first ('[' : '[' : xs) | [(a, xs')] <- (reads xs :: [(Integer, String)]) = Just (a, xs')
      first _ = Nothing

      second :: Maybe (Integer, String) -> Maybe ((Integer, Integer), String)
      second (Just (a, ',': xs)) | [(b, xs')] <- (reads xs :: [(Integer, String)]) = Just ((a, b), xs')
      second _ = Nothing

      third :: Maybe ((Integer, Integer), String) -> Maybe ((Integer, Integer), Integer, String)
      third (Just (ab, ']' : ',' : '[' : xs)) | [(x, xs')] <- (reads xs :: [(Integer, String)]) = Just (ab, x, xs')
      third _ = Nothing

      fourth :: Maybe ((Integer, Integer), Integer, String) -> Maybe ((Integer, Integer), (Integer, Integer), String)
      fourth (Just (ab, x, ',' : xs)) | [(y, xs')] <- (reads xs :: [(Integer, String)]) = Just (ab, (x, y), xs')
      fourth _ = Nothing

      fifth :: Maybe ((Integer, Integer), (Integer, Integer), String) -> Maybe (Mat2x2, String)
      fifth (Just (ab, xy, ']': ']' : xs)) = Just (MkMat (ab, xy), xs)
      fifth _ = Nothing

{-# LANGUAGE  FlexibleInstances #-}
module Cis194.Hw.Calc where

import  Cis194.Hw.ExprT
import  Cis194.Hw.Parser
import  qualified Cis194.Hw.VarExprT as VarExprT
import  qualified Data.Map as M
--import  Cis194.Hw.StackVM

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr x =
  case (parseExp Lit Add Mul x) of
    Nothing -> Nothing
    (Just y) -> Just (eval y)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit a = Lit a
  add a b = Add a b
  mul a b = Mul a b

instance Expr Integer where
  lit a = a
  add a b = a + b
  mul a b = a * b

instance Expr MinMax where
  lit a = MinMax a
  add x@(MinMax a) y@(MinMax b) = if a > b then x else y
  mul x@(MinMax a) y@(MinMax b) = if a > b then y else x

instance Expr Mod7 where
  lit a = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

instance Expr Bool where
  lit a = (a > 0)
  add a b = a || b
  mul a b = a && b

instance Expr String where
  lit a = show a
  add a b = concat a b

class HasVars a where
  var :: String -> a

--VarExprT instance methods
instance Expr VarExprT.VarExprT where
  lit a = VarExprT.Lit a
  add a b = VarExprT.Add a b
  mul a b = VarExprT.Mul a b

instance HasVars VarExprT.VarExprT where
  var a = VarExprT.Var a

--M.Map instance methods
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = (\_ -> Just a)
  add a b = \x -> case (a x) of
    Nothing -> Nothing
    Just y -> case (b x) of
      Nothing -> Nothing
      Just z -> Just (y + z)
  mul a b = \x -> case (a x) of
    Nothing -> Nothing
    Just y -> case (b x) of
      Nothing -> Nothing
      Just z -> Just (y * z)

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var a = M.lookup a

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


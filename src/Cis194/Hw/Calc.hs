module Cis194.Hw.Calc where

import  Cis194.Hw.ExprT
import  Cis194.Hw.Parser

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add (Lit a) (Lit b)) = a + b
eval (Add (Lit a) b) = eval (Add (Lit a) (Lit (eval b)))
eval (Add a (Lit b)) = eval (Add (Lit (eval a)) (Lit b))
eval (Mul (Lit a) (Lit b)) = a * b
eval (Mul (Lit a) b) = eval (Mul (Lit a) (Lit (eval b)))
eval (Mul a (Lit b)) = eval (Mul (Lit (eval a)) (Lit b))

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

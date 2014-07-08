module Cis194.Hw.Calc where 

import  Cis194.Hw.ExprT

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)

evalStr :: String -> Maybe Integer
evalStr _ = Nothing

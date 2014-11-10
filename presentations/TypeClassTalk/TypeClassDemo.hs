
{-# LANGUAGE FlexibleInstances #-}

module TypeClassDemo where

{-
fix :: Int -> String
fix i = show (i + 600)

fix :: Float -> String -- ghc: Duplicate type signatures for ‘fix’
fix f = show (100 * f) -- ghc: Multiple declarations of ‘fix’
-}

class Fixer a where fix :: a -> String

instance Fixer Int    where fix i = show (i + 600)
instance Fixer Float  where fix f = show (100 * f)
instance Fixer Char   where fix c = c:['X', '1']
instance Fixer String where fix s = s

i =    1 :: Int
f = 6.01 :: Float

main = do putStrLn $ show (fix i, fix f, fix '6', fix "601")

-- ("601","601.0","6O1","601")

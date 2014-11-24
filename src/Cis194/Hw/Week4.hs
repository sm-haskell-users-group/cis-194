module Cis194.Hw.Week4 where

import Cis194.Hw.BST

impossible :: a
impossible = undefined

-- All we can do is return the second parameter
-- AKA: snd
ex1 :: a -> b -> b
ex1 _ x = x

ex2 :: a -> a -> a
ex2 _ _ = undefined

ex3 :: Int -> a -> a
ex3 _ _ = undefined

ex4 :: Bool -> a -> a -> a
ex4 _ _ _ = undefined

ex5 :: Bool -> Bool
ex5 _ = undefined

ex6 :: (a -> a) -> a
ex6 _ = undefined

ex7 :: (a -> a) -> a -> a
ex7 _ _ = undefined

ex8 :: [a] -> [a]
ex8 _ = undefined

ex9 :: (a -> b) -> [a] -> [b]
ex9 _ _ = undefined

ex10 :: Maybe a -> a
ex10 _ = undefined

ex11 :: a -> Maybe a
ex11 _ = undefined

ex12 :: Maybe a -> Maybe a
ex12 _ = undefined

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ _ _ = undefined

allCaps :: [String] -> Bool
allCaps _ = undefined

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace _ = undefined

firstLetters :: [String] -> [Char]
firstLetters _ = undefined

asList :: [String] -> String
asList _ = undefined

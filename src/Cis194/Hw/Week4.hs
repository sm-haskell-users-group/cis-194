module Cis194.Hw.Week4 where

import Cis194.Hw.BST

impossible :: a
impossible = undefined

-- All we can do is return the second parameter
-- AKA: snd
ex1 :: a -> b -> b
ex1 _ x = x

-- We can return either argument, but we can't do anything else.
ex2 :: a -> a -> a
ex2 _ x = x
-- ex2 x _ = x

-- We have some info about the first parameter, but none about the second.
-- As a result, we can't combine these types, and are forced to drop the first.
ex3 :: Int -> a -> a
ex3 _ x = x

-- This is the first exercise where we have real flexibility of interface.
-- I chose to implement a Bool-influenced fst/snd kind of function
ex4 :: Bool -> a -> a -> a
ex4 b x y = if b then x else y

-- We have four options here: Static values, identity, or invert.
ex5 :: Bool -> Bool
ex5 _ = True
-- ex5 _ = False
-- ex5 b = b
-- ex5 b = not b

-- As we have no 'a's to start with, we can't do anything.
ex6 :: (a -> a) -> a
ex6 _ = impossible

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

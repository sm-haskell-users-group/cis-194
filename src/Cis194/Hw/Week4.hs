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

-- As opposed to ex6, now we've got an 'a'! We have two options:
-- transform, or not.
ex7 :: (a -> a) -> a -> a
ex7 f x = f x
-- ex7 _ x = x

-- We have some flexibility here: Because we can't reason about the list, we
-- can't get the head or tail or anything-- We can still return the unmodified
-- list, we can reverse the list, and we can return an empty list.
--
-- Unfortunately, reverse only works on finite lists, so this is not complete.
ex8 :: [a] -> [a]
ex8 xs = foldl (\a x -> x : a) [] xs
-- ex8 xs = xs
-- ex8 _ = []

-- This is our friend (f)map!
-- Because we have to provide alternate implementations, let's just return [].
-- AKA: fmap
ex9 :: (a -> b) -> [a] -> [b]
ex9 f xs = foldr (\x a -> f x : a) [] xs
-- ex9 _ _ = []

-- We can't reason about the type or whether this is Just or Nothing, so this
-- is impossible.
ex10 :: Maybe a -> a
ex10 _ = impossible

-- We have two options here, utilize 'a' or dump it.
ex11 :: a -> Maybe a
ex11 x = Just x
-- ex11 _ = Nothing

-- We've got two options here as well, dump the value or return it as-is
ex12 :: Maybe a -> Maybe a
ex12 _ = Nothing
-- ex12 x = x

-- I wonder if there's a way to use Ord more effectively than this?
-- The definition of build here reduces a lot of repetition
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _   x Leaf = Node Leaf x Leaf
insertBST cmp x bst@(Node l v r)
  | ord == LT = Node (build l) v r
  | ord == GT = Node l v (build r)
  | otherwise = bst
    where ord = cmp x v
          build = insertBST cmp x

allCaps :: [String] -> Bool
allCaps _ = undefined

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace _ = undefined

firstLetters :: [String] -> [Char]
firstLetters _ = undefined

asList :: [String] -> String
asList _ = undefined

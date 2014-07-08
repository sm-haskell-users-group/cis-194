module Cis194.Hw.Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . (map $ \x -> x - 2) . (filter even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = 1 * n + fun2 (n `div` 2)
    | otherwise = 0 * n + fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n = if even n then fun2'e n else fun2'o n

fun2'e n = n + fun2' (n `div` 2)
fun2'o n = fun2' $ 3 * n + 1

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) = Leaf

-- Odd number of True values
xor :: [Bool] -> Bool
xor = odd . length . filter id

map' :: (a -> b) -> [a] -> [b]
-- map' f xs = foldl (\a x -> (f x) : a) [] xs
-- map' f = foldl (\a x -> (f x) : a) []
map' = (flip $ foldr . (\f x a -> f x : a)) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram _ = []

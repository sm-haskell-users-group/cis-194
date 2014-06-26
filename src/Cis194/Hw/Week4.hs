module Cis194.Hw.Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' x = product . map (+(-2)) $ filter (even) x

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ filter (even) $ takeWhile (>1) $ iterate (\x -> if (even) x then x `div`2 else 3 * x + 1) n

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree _ = Leaf

xor :: [Bool] -> Bool
xor x = odd (length $ filter (==True) x)

xor' x = odd $ foldl (+) 0 [length $ (filter (==True) x)] -- it's not pretty, but it's using a fold as required :p

map' :: (a -> b) -> [a] -> [b]
map' f z = foldr (\x acc -> f x : acc) [] z

sieveSundaram :: Integer -> [Integer]
sieveSundaram _ = []

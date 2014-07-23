module Cis194.Hw.Week4 where
import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x-2) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate fun2Alg

fun2Alg :: Integer -> Integer
fun2Alg n
  | n == 2 = 1
  | odd n = 3 * n + 1
  | even (n `div` 2) = n `div` 2
  | otherwise = (3 * (n `div` 2) + 1)

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
--foldTree [x] = Node 0 Leaf x Leaf
--foldTree xs = foldr (flip createTree) Leaf xs

xor :: [Bool] -> Bool
xor = odd . foldl (\y z -> if z then y+1 else y) 0

map' :: (a -> b) -> [a] -> [b]
map' x xs = foldr (\z y -> (x z) : y) [] xs

map2' :: (a -> b) -> [a] -> [b]
map2' x xs = foldl (\y z -> y ++ [(x z)]) [] xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2*x + 1) . sundaramList

sundaramList :: Integer -> [Integer]
sundaramList n = [1..n] \\ [i+j+2*i*j | i <- [1..n], j <- [1..n], i <= j && (i + j + 2*i*j) < n && ] n

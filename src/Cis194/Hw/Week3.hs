module Cis194.Hw.Week3 where

{- part 1 -}
skips :: [a] -> [[a]]
skips a = xlate a (map(retainMod [1,2..(length a)]) [1,2..(length a)])

xlate :: [a] -> [[Int]] -> [[a]]
xlate a xxs = [map (a !!) xs | xs <- xxs]


retainMod :: [Int] -> Int -> [Int]
retainMod [] m = []
retainMod (n:xs) m =
    if ((mod n m)) == 0
        then n-1 : (retainMod xs m)
        else retainMod xs m

{- part 2 -}

localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima (x:xs) =
    if (length xs > 1) && (x < (xs!!0) && (xs!!0) > (xs!!1))
        then xs!!0 : (localMaxima xs)
        else localMaxima xs

{- part3 -}

histogram :: [Integer] -> String
histogram = foldr (\ x xs -> (histoAccu x x)

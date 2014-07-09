module Cis194.Hw.Golf where

import Data.List

skips :: [a] -> [[a]]
skips [] = []
-- y is an array of tuples - [("h", 1), ("e", 2)]
skips x =
  let y = (zip x [1..])
  in foldl (\a (b, c) -> a ++ [(f y c)]) [] y

-- f mods based on an integer than reduces the tuple to the original element
f :: [(a, Integer)] -> Integer -> [a]
f x y = map fst $ filter (\(a, b) -> (mod b y) == 0) x

-- a encapsulates the rest of the list after the first element
localMaxima :: [Integer] -> [Integer]
localMaxima (x:a@(y:z:_)) =
  if y > x && y > z then [y] ++ localMaxima a else localMaxima a
localMaxima _ = []

-- transpose $ group $ sort x is creating an array of arrays with unique groupings of values.
-- 'c' creates a the string row based on one grouping. these need to be reversed because the transpose method fills in from left to right.
histogram :: [Integer] -> String
histogram x = foldl c "==========\n0123456789\n" (transpose $ group $ sort x)

-- c just builds a row of the histogram
c :: String -> [Integer] -> String
c a x =
  (foldl (\y z -> let (ys, _:zs) = splitAt z y in ys ++ "*" ++ zs) "          \n" (map fromIntegral x)) ++ a

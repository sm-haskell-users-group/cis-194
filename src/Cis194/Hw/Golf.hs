module Cis194.Hw.Golf where

import Data.Maybe

mapWithIndex :: ((Int, a) -> Maybe a) -> [a] -> [a]
mapWithIndex f xs = mapMaybe f $ zip [1..] xs

takeEvery :: Int -> [a] -> [a]
takeEvery n xs
    | n > 0 = mapWithIndex (\(i, x) -> if i `mod` n == 0 then Just x else Nothing) xs
    | otherwise = []

skips :: [a] -> [[a]]
skips xs = zipWith takeEvery [1.. (length xs)] (cycle [xs])

localMaxima :: [Integer] -> [Integer]
localMaxima _ = []

histogram :: [Integer] -> String
histogram _ = ""

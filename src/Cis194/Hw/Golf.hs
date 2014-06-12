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

sliding :: Int -> [a] -> [[a]]
sliding i xs = map (\x -> take i $ drop x xs) [0..(l - i)]
    where l = length xs

localMaxima :: [Integer] -> [Integer]
localMaxima xs
    | length xs < 3 = []
    | otherwise = map (!! 1) $ filter (\l -> maximum l == l !! 1) $ sliding 3 xs

histogram :: [Integer] -> String
histogram _ = ""

module Cis194.Hw.Golf where

skips :: [a] -> [[a]]
skips [] = []
skips t  = [t]
skips [t] = [[t]]


localMaxima :: [Integer] -> [Integer]
localMaxima xs
    | length xs < 3 = []
    | otherwise = map (!! 1) $ filter (\l -> maximum l == l !! 1) $ map (\x -> take 3 $ drop x xs) [0..(l - 3)] where l = length xs


histogram :: [Integer] -> String
histogram _ = ""

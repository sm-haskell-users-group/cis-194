module Cis194.Hw.Golf where

import Data.Maybe
import Data.List
import qualified Data.HashMap.Lazy as LazyMap

-- skips utilities

mapWithIndex :: ((Int, a) -> Maybe a) -> [a] -> [a]
mapWithIndex f xs = mapMaybe f $ zip [1..] xs

takeEvery :: Int -> [a] -> [a]
takeEvery n xs
    | n > 0 = mapWithIndex (\(i, x) -> if i `mod` n == 0 then Just x else Nothing) xs
    | otherwise = []

skips :: [a] -> [[a]]
skips xs = zipWith takeEvery [1.. (length xs)] $ repeat xs

-- localMaxima utilities

sliding :: Int -> [a] -> [[a]]
sliding i xs = map (\x -> take i $ drop x xs) [0..(l - i)]
    where l = length xs

localMaxima :: [Integer] -> [Integer]
localMaxima xs
    | length xs < 3 = []
    | otherwise = map (!! 1) $ filter (\l -> maximum l == l !! 1) $ sliding 3 xs

localMaxima' m = concatMap (\[x,y,z] -> if y > x && y > z then [y] else []) [ x | x <- map (take 3) $ tails m, length x == 3]
localMaxima'' m = mapMaybe (\[x,y,z] -> if y > x && y > z then Just y else Nothing) [ x | x <- map (take 3) $ tails m, length x == 3]

-- histogram utilities

rep :: Integer -> a -> [a]
rep _i ch = take i $ repeat ch
    where i = fromInteger _i

hbuild :: LazyMap.HashMap Integer Integer
hbuild = LazyMap.fromList $ zip [0..9] $ repeat 0

hgroup :: [Integer] -> [[Integer]]
hgroup = Data.List.group . Data.List.sort

hupdate :: [Integer] -> LazyMap.HashMap Integer Integer -> LazyMap.HashMap Integer Integer
hupdate xs hm = LazyMap.insert (head xs) (toInteger $ length xs) hm

hflatten :: LazyMap.HashMap Integer Integer -> [[String]]
hflatten hm = LazyMap.foldlWithKey' (\a k v -> ([show k, "="] ++ rep v "*" ++ rep (max - v) " ") : a) [] hm
    where max = maximum $ LazyMap.elems hm

histogram :: [Integer] -> String
histogram xs = unlines $ map (reverse . foldl (++) "") $ reverse $ transpose $ hflatten $ foldr hupdate hbuild $ hgroup xs

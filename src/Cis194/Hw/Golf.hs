module Cis194.Hw.Golf where

import Data.List

skips :: [a] -> [[a]]
skips xs = takeWhile (\x -> not (null x)) [(skipsN n xs) | n <- [1..]]

skipsN :: Int -> [a] -> [a]
skipsN _ [] = []
skipsN n xs = if (length (snd parts)) >= n
                then (last (fst parts)) : (skipsN n (snd parts))
                else if ((length (fst parts)) >= n)
                    then (last (fst parts)) : []
                    else []
    where parts = splitAt n xs

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = if ((b > a) && (b > c))
                         then b : (localMaxima (b:c:xs))
                         else (localMaxima (b:c:xs))
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = intercalate "\n" ((map toStar (reverse (transpose (groupBy (==) (sort xs))))) ++ ["==========", "0123456789", ""])
    where toStar xs = [j | i <- [0..9], let j = (if (elem i xs) then '*' else ' ')]

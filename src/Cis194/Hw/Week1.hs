module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

lastDigit :: Integer -> Integer
lastDigit = flip mod 10

dropLastDigit :: Integer -> Integer
dropLastDigit = flip div 10

toDigits :: Integer -> [Integer]
toDigits = reverse . (map lastDigit) . takeWhile (> 0) . (iterate dropLastDigit)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = snd . iter
  where iter [] = (0, [])
        iter (x:xs) = (1 - times, (x * (times + 1)) : r)
          where (times, r) = iter xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . (concatMap toDigits)

validate :: Integer -> Bool
validate = (== 0) . (flip mod 10) . sumDigits . doubleEveryOther . toDigits

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi discs from to swap = hanoi (discs - 1) from swap to ++ (from, to) : hanoi (discs - 1) swap to from

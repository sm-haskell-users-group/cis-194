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
sumDigits _ = undefined

validate :: Integer -> Bool
validate _ = undefined

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = undefined

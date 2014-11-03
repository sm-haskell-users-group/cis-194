module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

lastDigit :: Integer -> Integer
lastDigit = flip mod 10

dropLastDigit :: Integer -> Integer
dropLastDigit = flip div 10

toDigits :: Integer -> [Integer]
toDigits _ = undefined

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther _ = undefined

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

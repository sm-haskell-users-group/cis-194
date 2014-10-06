module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0     = toDigits (dropLastDigit n) ++ [lastDigit n]
    | otherwise = []

doubleSecond :: [Integer] -> [Integer]
doubleSecond []         = []
doubleSecond (x:[])     = [x]
doubleSecond (x:(y:zs)) = x : (2 * y) : doubleSecond zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = reverse (doubleSecond (reverse lst))

sumDigits :: [Integer] -> Integer
sumDigits lst = sum (concatMap toDigits lst)

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDiscs a b c
    | numDiscs == 0 = []
    | numDiscs == 1 = [(a, b)]
    | otherwise     = (hanoi (numDiscs - 1) a c b) ++ [(a, b)] ++ (hanoi (numDiscs - 1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 numDiscs a b c d
    | numDiscs == 0 = []
    | numDiscs == 1 = [(a, b)]
    | otherwise     = undefined

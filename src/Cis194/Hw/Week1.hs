module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

lastDigit :: Integer -> Integer
lastDigit n = mod n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

toDigits :: Integer -> [Integer]
toDigits n | n < 1 = []
           | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = snd $ foldr (\x (bit, acc) ->
  ((if bit == 2 then 1 else 2), ((x * bit) : acc))) (1, []) xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = 0 == sumDigits (doubleEveryOther (toDigits n)) `mod` 10

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _    = []
hanoi 1 p1 p2 _  = [(p1, p2)]
hanoi n p1 p2 p3 = (hanoi (n-1) p1 p3 p2) ++ [(p1, p2)] ++ (hanoi (n-1) p3 p2 p1)

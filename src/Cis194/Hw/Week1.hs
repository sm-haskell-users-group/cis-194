module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse
  where doubleEveryOther' (x:y:ys) = x : y*2 : doubleEveryOther' ys
        doubleEveryOther' x = x

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate n = s `mod` 10 == 0
  where s = sumDigits . doubleEveryOther $ toDigits n

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 0 = []
  | otherwise = hanoi (n-1) a c b
                ++ [(a,b)]
                ++ hanoi (n-1) c b a

-- tower of hanoi with extra peg

-- 1: move n-k to d (the extra)
-- 2: move the remaining k to b, using normal hanoi
-- 3: move n-k back from d to b
-- k is chosen such that n is the kth triangle number

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
 | n == 0 = []
 | otherwise  = hanoi4 (n-k) a d b c
                ++ hanoi k a b c
                ++ hanoi4 (n-k) d b a c
  where k = head [x | x <- [1..], x * (x + 1) `div` 2 >= n]

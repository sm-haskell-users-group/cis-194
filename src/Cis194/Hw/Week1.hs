module Cis194.Hw.Week1 where
import Data.Char

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits nr = [ fromIntegral (digitToInt c) | c <- show nr, 
                    isNumber c, nr > 0 ]

toDigitsRev :: Integer -> [Integer]
toDigitsRev nr = reverse $ toDigits nr

doubleEveryOther_ :: [Integer] -> [Integer]
doubleEveryOther_  [] = []
doubleEveryOther_  [x] = [x]
doubleEveryOther_  (x:y:xs) = x : (2 * y) : doubleEveryOther_ xs
doubleEveryOther xs = reverse $ doubleEveryOther_  $ reverse xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate nr  = mod (sumDigits $ doubleEveryOther $ toDigits nr ) 10 == 0
---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []

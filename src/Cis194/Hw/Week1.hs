module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits cardNumber =
  reverse $ toDigitsRev cardNumber

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev cardNumber =
  if
    cardNumber < 0 then []
  else
    cardNumber `mod` 10 : toDigitsRev(cardNumber `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : 2*y : doubleEveryOther zs

sumIfGreaterThanNine :: Integer -> Integer
sumIfGreaterThanNine n =
  if
    (n `div` 10) == 0 then n
  else
    sum(toDigits n)

sumDigits :: [Integer] -> Integer
sumDigits listOfDigits =
  sum $ map sumIfGreaterThanNine listOfDigits

checkSum :: Integer -> Bool
checkSum n = (n `mod` 10) == 0

validate :: Integer -> Bool
validate n =
  checkSum . sumDigits . doubleEveryOther $ toDigitsRev n

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []

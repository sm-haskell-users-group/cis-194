module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = (toDigits (dropLastDigit x)) ++ [lastDigit x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = foldl (\acc (a,b) -> acc ++ [if a then b*2 else b]) [] (zip [i| _ <- [1..], i <- [False,True]] xs)

sumDigits :: [Integer] -> Integer
sumDigits xs = foldr (\x acc -> acc + if x < 10 then x else (sum . toDigits) x) 0 xs

validate :: Integer -> Bool
validate x = x `div` 10 == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDisc p1 p2 p3
    | numDisc == 1 = [(p1, p2)]
    | numDisc > 1 = (hanoi (numDisc-1) p1 p3 p2) ++ (hanoi 1 p1 p2 p3) ++ (hanoi (numDisc-1) p3 p2 p1)
    | otherwise = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = undefined

module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x = toDigitsTCORec x []

toDigitsTCORec :: Integer -> [Integer] -> [Integer]
toDigitsTCORec x xs
    | x <= 0    = xs
    | otherwise = toDigitsTCORec (x `div` 10) xs ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (map
                        (\(x, y) -> x * y)
                        (zip (cycle [1,2]) (reverse xs)))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = ((sumDigits (doubleEveryOther (toDigits x))) `mod` 10) == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 from to _ = [(from, to)]
hanoi size from to swap = (hanoi (size - 1) from swap to) ++ (hanoi 1 from to swap) ++ (hanoi (size - 1) swap to from)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 from to _ _ = [(from, to)]
hanoi4 2 from to swap _ = [(from, swap), (from, to), (swap, to)]
hanoi4 3 from to swap1 swap2 = [(from, swap1), (from, swap2), (from, to), (swap2, to), (swap1, to)]
hanoi4 size from to swap1 swap2 = (hanoi4 (size - 1) from swap1 to swap2) ++ (hanoi4 1 from to swap1 swap2) ++ (hanoi4 (size - 1) swap1 to from swap2)

module Cis194.Hw.Week1
( toDigits
, toDigitsRev
, doubleEveryOther
, sumDigits
, validate
, hanoi
) where


{- Credit Card Number Validation
 - double the value of every second digit beginning from the right
 - add the digits of the doubled values and the undoubled digits from the original number
 - calculate the remainder when the sum is divided by 10: if the result equals 0, the number is valid
-}
toDigits :: Integer -> [Integer]
toDigits x
    | x > 0 = toDigits (x `div` 10) ++ [x `mod` 10]
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x > 0 = [x `mod` 10] ++ toDigitsRev (x `div` 10)
    | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = reverse (map (\(x, y) -> x * y) $ zip (cycle [1,2]) (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum $ toDigits x
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x
    | (sumDigits $ doubleEveryOther $ toDigits x) `mod` 10 == 0 = True
    | otherwise = False


{- Towers of Hanoi
 - Given the number of discs and names for the three pegs, hanoi
    should return a list of moves to be performed to move the stack of
    discs from the first peg to the second.
 - Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

optimalSolution :: Integer -> Integer
optimalSolution discs = 2^discs - 1
-- an optimal hanoi game with 5 discs and three pegs should take 31 moves

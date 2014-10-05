module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

lastDigit :: Integer -> Integer
lastDigit = flip rem 10

dropLastDigit :: Integer -> Integer
dropLastDigit = flip div 10

toDigits :: Integer -> [Integer]
-- (n <= 0) = []
toDigits = ƒ [] where
  ƒ ß n | (1 > n) = ß
  ƒ ß n = ƒ (lastDigit n : ß) (dropLastDigit n)

toDigits' ccNum = if (ccNum <= 0) then [] else (ƒ ccNum []) where
  ƒ 0 ß = ß -- a leading kind of zero!
  ƒ n ß = ƒ (dropLastDigit n) (lastDigit n : ß) -- 123 ß = 12 3:ß

doubleEveryOther :: [Integer] -> [Integer]
-- double every second digit beginning from the right!
doubleEveryOther ns = if null ns then [] else ƒ (reverse ns) [] where
  ƒ (n:(d:ns')) ß = ƒ ns' (d + d : n : ß) -- ns was [..,d,n]
  ƒ       ns'   ß =   ns' ++ ß

  ƒ' (n:_) ß = n : ß
  ƒ' [   ] ß = ß

sumDigits :: [Integer] -> Integer
sumDigits ns = sum $ ns >>= toDigits

sumDigits' = foldr ((+) . (sum . toDigits)) 0
sumDigits'' = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate ccNum = rem (checksum ccNum) 10 == 0 where
  checksum = sumDigits . doubleEveryOther . toDigits

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- move n discs from the first peg to the second
{- hanoi 2 "a" "b" "c" = [("a","c"), ("a","b"), ("c","b")] -}
hanoi n _ _ _ | 1 > n = []
hanoi 1 ab ad _ = [(ab, ad)] -- optional optimization?
hanoi n ab ad tmp = let n' = n - 1 in
  (hanoi n' ab tmp ad) ++ (ab, ad) : (hanoi n' tmp ad ab)


{--- extras ---}

hanoid d = hanoi d "a" "b" "c"

type Hanoi = ([Int], [Int], [Int]) -- a game state (3 towers)

build :: Int -> Hanoi
build n = ([1..n], [], [])

move :: Hanoi -> Move -> Hanoi
move (t1, t2, t3) m = ƒ m where
  get tx = tail tx
  put tx ty = head tx : ty
  ƒ ("a", "b") = (   get t1, put t1 t2,        t3)
  ƒ ("a", "c") = (   get t1,        t2, put t1 t3)
  ƒ ("b", "a") = (put t2 t1,    get t2,        t3)
  ƒ ("b", "c") = (       t1,    get t2, put t2 t3)
  ƒ ("c", "a") = (put t3 t1,        t2,    get t3)
  ƒ ("c", "b") = (       t1, put t3 t2,    get t3)

towers :: [Hanoi] -> [Move] -> [Hanoi]
towers ts [] = reverse ts
towers ts (m:ms) = let t' = move (head ts) m in towers (t':ts) ms

drawLn :: Hanoi -> String
drawLn (t1, t2, t3) =
  "\n  " ++ draw t1 ++ "•    " ++ draw t2 ++ "•    " ++ draw t3 ++ "•\n" where
    width = (sum $ map (\t -> sum t + length t) [t1, t2, t3])
    pad t = replicate (width - sum t - length t) ' '
    draw t = (pad t) ++ (t >>= \i -> replicate i '◊' ++ " ")

towersN :: Integer -> [Hanoi]
towersN n = towers [build (fromIntegral n)] (hanoid n)

test n = putStrLn $ towersN n >>= drawLn
tests = putStrLn $ [0..4] >>=
  \i -> ((show i ++ " ––––") ++) . (>>= drawLn) . towersN $ i

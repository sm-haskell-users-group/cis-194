module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

-- quest to avoid explicit parameters

lastDigit :: Integer -> Integer
lastDigit = flip rem 10

dropLastDigit :: Integer -> Integer
dropLastDigit = flip div 10

toDigits :: Integer -> [Integer] -- 0 or negative = []
toDigits = ƒ [] where ƒ ß n |  (1 > n)  = ß
                            | otherwise = ƒ (d:ß) q where (q, d) = quotRem n 10

toDigits' ccNum = if (ccNum <= 0) then [] else (ƒ [] ccNum) where
  ƒ ß 0 = ß -- a leading kind of zero!
  ƒ ß n = ƒ (lastDigit n : ß) (dropLastDigit n) -- ß 123 = 3:ß 12

doubleEveryOther :: [Integer] -> [Integer] -- second last first!
doubleEveryOther = ƒ [] . reverse where
  ƒ ß (n:(d:ns)) = ƒ (d + d : n : ß) ns -- was [..,d,n]
  ƒ ß       ns   = ns ++ ß

sumDigits :: [Integer] -> Integer
sumDigits = sum . (>>= toDigits)

validate :: Integer -> Bool
validate = (0 ==) . lastDigit . sumDigits . doubleEveryOther . toDigits

validate' ccNum = rem (checksum ccNum) 10 == 0 where
  checksum = sumDigits . doubleEveryOther . toDigits

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- move n discs from the first peg to the second
hanoi n _ _ _ | 1 > n = [] -- check n <= 0 only once!
hanoi n a b c         = ƒ n a b c
  where
    ƒ 1 a b _ =               [ (a, b) ]
    ƒ n a b c = (ƒ n' a c b) ++ (a, b) : (ƒ n' c b a) where n' = pred n

hanoi' n ab ad tmp |     1 > n = []
                   |    1 == n = [(ab, ad)] -- optional optimization?
                   | otherwise = let n' = n - 1 in
                       (hanoi' n' ab tmp ad) ++ (ab, ad) : (hanoi' n' tmp ad ab)

hanoi'' n _ _ _ | 1 > n = []
hanoi'' 1 ab ad _ = [(ab, ad)] -- optional optimization?
hanoi'' n ab ad tmp = let n' = n - 1 in
  (hanoi'' n' ab tmp ad) ++ (ab, ad) : (hanoi'' n' tmp ad ab)

{--- extras ---}

hanoid = \d -> hanoi d "a" "b" "c"

type Hanoi = ([Int], [Int], [Int]) -- a game state (3 towers)

build n = ([1..fromIntegral n], [], [])

move :: Hanoi -> Move -> Hanoi
move (t1, t2, t3) m = ƒ m where
  get = tail
  put t = (head t :)
  ƒ ("b", "a") = (put t2 t1,    get t2,        t3)
  ƒ ("c", "a") = (put t3 t1,        t2,    get t3)
  ƒ ("a", "b") = (   get t1, put t1 t2,        t3)
  ƒ ("c", "b") = (       t1, put t3 t2,    get t3)
  ƒ ("a", "c") = (   get t1,        t2, put t1 t3)
  ƒ ("b", "c") = (       t1,    get t2, put t2 t3)

hanois :: Integer -> [Hanoi]
hanois n = scanl move (build n) (hanoid n)

-- is this in Prelude?
fgx :: (a -> b) -> (a -> b) -> a -> [b]
fgx f g x = [f x, g x]

drawLn :: Hanoi -> String
drawLn (t1, t2, t3) = '\n' : (¶) [t1, t2, t3] "\n" where
  (¶) ts = flip (foldr (¶)) ts where
    width = 3 + sum (ts >>= fgx sum length)
    (¶) t = (pad t) . (draw t) . ("•" ++) where
      pad  t = (++) (replicate (width - sum t - length t) ' ')
      draw t = (++) (t >>= \i -> replicate i '◊' ++ " ")

test n = putStrLn (hanois n >>= drawLn)

tests = putStrLn $ [0..4] >>=
  \n -> '\n' : show n ++ " ––––" ++ (hanois n >>= drawLn)

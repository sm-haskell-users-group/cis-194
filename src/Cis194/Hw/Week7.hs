module Cis194.Hw.Week7 where

import System.Random

-- e01a: Define fib so that fib n computes the nth Fibonacci number Fn:
fib :: Integer -> Integer
fib _ = undefined

-- e01b: Now use fib to define the infinite list of all Fibonacci numbers:
fibs1 :: [Integer]
fibs1 = undefined

-- e02: Define the infinite list fibs2 so that it has the same elements as
-- fibs1, but computing the first n elements of fibs2 requires only (roughly) n
-- addition operations:
fibs2 :: [Integer]
fibs2 = undefined

-- Stream is a data type that _must_ be infinite:
data Stream a = Cons a (Stream a)

-- e03: Write a function to convert a Stream to an infinite list:
streamToList :: Stream a -> [a]
streamToList _ = undefined

-- e04: Make your own instance of Show for Stream which works by showing only
-- some prefix of a stream (say, the first 20 elements)
instance Show a => Show (Stream a) where
  show _ = undefined

-- e05a: Write a function which generates a stream containing infinitely many
-- copies of the given element:
streamRepeat :: a -> Stream a
streamRepeat _ = undefined

-- e05b: Write a function which applies a function to every element of a
-- Stream:
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap _ _ = undefined

-- e05c: Write a function which generates a Stream from a "seed" of type a,
-- which is the first element of the stream, and an "unfolding rule" of type
-- a -> a which specifies how to transform the seed into a new seed, to be used
-- for generating the rest of the stream:
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed _ _ = undefined
-- Example: streamToList (streamFromSeed ('x' :) "o") == ["o", "xo", "xxo", "xxxo", "xxxxo", ... ]

-- e06a: Define the stream nats which contains the infinite list of natural numbers 0, 1, 2, ...
nats :: Stream Integer
nats = undefined

-- e06b: Define the stream ruler which corresponds to the "ruler function"
ruler :: Stream Integer
ruler = undefined

-- e07: Write a function that produces an infinite pseudo-random sequence,
-- given a generator of type g:
randomList :: (Random a, RandomGen g) => g -> [a]
randomList _ = undefined
-- The `random` function will be helpful.

-- e08: Write a function randomInts such that randomInts n is a pseudo-random
-- sequence of Ints, with length n.
randomInts :: Int -> [Int]
randomInts _ = undefined

-- e09: Profile memory usage
--
--   ghc src/Cis194/Hw/Week7.hs -rtsopts -main-is Cis194.Hw.Week7
--   ./src/Cis194/Hw/Week7 +RTS -h -i0.001
--   hp2ps -c Week7.hp
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing -- no min or max if there are no elements
minMax xs = Just (minimum xs, maximum xs)

-- e10: Profile memory usage pt. 2
-- Implement this better version of minMax that takes advantage of laziness,
-- then run again with +RTS -s, and include the improved memory footprint
-- (the "total memory in use" is the one that matters!) in a comment.

-- e11 (Optional):
--   - Create a type Matrix which represents 2 × 2 matrices of Integers
--
--   - Make an instance of the Num type class for Matrix. In fact, you only
--     have to implement the (*) method, since that is the only one we
--     will use. (If you want to play around with matrix operations a bit
--     more, you can implement fromInteger, negate, and (+) as well.)
--
--   - We now get fast (logarithmic time) matrix exponentiation for free,
--     since (^) is implemented using a binary exponentiation algorithm
--     in terms of (*).
--   - Write a function fib4 which computes the nth Fibonacci number by raising
--     F to the nth power and projecting out Fn (you will also need a special
--     case for zero). Try computing the one millionth or even ten millionth
--     Fibonacci number.

fib4 :: Integer -> Integer
fib4 _ = undefined

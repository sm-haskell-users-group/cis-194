module Cis194.Hw.Fibonacci where

----------
-- Ex 1 --
----------

-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function definition of type:
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-2)) + (fib (n-1))

--
-- so that fib n computes the nth Fibonacci number Fn. Then, use fib to
-- define the infinite list of all Fibonacci numbers:
--
-- fibs1 :: [Integer]

fibs1 :: [Integer]
fibs1 = map fib [0..]

----------
-- Ex 2 --
----------

-- Define the infinite list:
--
fibs2 :: [[Integer]]
fibs2 = iterate (\z@(x:y:_) -> (x+y) : z) [1, 1, 0]

-- a  b  c
-- 0, 1, 1, 2, 3, 5, 8

--[0, 1, 1]
--next iteration = (a:b:c:ds) b + c
-- iterate

--dubble x = x * 2
--take 5 $ iterate computeNext [0, 1, 1]
-- [4, 8, 16, 32, 64]



--
-- so that it has the same elements as fibs, but computing the first n
-- elements of fibs2 requires only O(n) addition operations. Be sure to
-- use standard recursion pattern(s) from Prelude, as appropriate.
--


----------
-- Ex 3 --
----------

--data List a = Nil | Cons a (List a)

data Stream a = Cons a (Stream a)

-- * Define a data type of polymorphic streams, Stream.
-- * Write a function to convert a Stream to an infinite list:
--

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y
--
-- * Make your own instance of Show for Stream:
--
instance Show a => Show (Stream a) where
  show x = show $ take 20 (streamToList x)
--
--   ...which works by showing only some prefix of a stream (say, the first
--   20 elements)

----------
-- Ex 4 --
----------
-- * Write a function:
--
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

--
-- ...which generates a stream containing infinitely many copies of the
-- given element
--
-- * Write a function:
--
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f(x)) (streamMap f y)
--
-- ...which applies a function to every element of a Stream
--
-- * Write a function:
--
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f(x)))
--
-- ...which generates a Stream from a “seed” of type a, which is the first
-- element of the stream, and an “unfolding rule” of type a -> a which
-- specifies how to transform the seed into a new seed, to be used for
-- generating the rest of the stream.

----------
-- Ex 5 --
----------

-- * Define the stream:
--
nats :: Stream Integer
nats = streamFromSeed (\x -> x+1) 0
--
-- ...which contains the infinite list of natural numbers 0, 1, 2...
--

interleaveStreams :: a -> Stream a -> Stream a
interleaveStreams a (Cons x y) = Cons a (Cons x (interleaveStreams a y))
-- * Define the stream:
--
ruler :: Stream Integer
ruler = streamMap largestPowerOfTwo $ streamFromSeed (+1) 1
--

largestPowerOfTwo :: Integer -> Integer
largestPowerOfTwo x
  | odd x = 0
  | otherwise = 1 + largestPowerOfTwo (x `div` 2)
-- ...which corresponds to the ruler function:
--
-- 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
--
-- ...where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly divides n.
--
-- Hint: define a function interleaveStreams which alternates the
-- elements from two streams. Can you use this function to implement ruler
-- in a clever way that does not have to do any divisibility testing?

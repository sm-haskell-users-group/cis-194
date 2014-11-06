module Week4 where

import BST
import Data.Char
import Data.List
import Data.Maybe

--ex 1

ex1 :: a -> b -> b
ex1 _ x = x
--ex1 = curry snd

-- This is the only possible implementation of this type as written - if we knew
-- that b was an instance of some type class, we could do more.

--ex 2

ex2 :: a -> a -> a
ex2 x _ = x
--ex2 = curry fst

-- or

ex2' :: a -> a -> a
ex2' _ x = x
--ex2' = curry snd

-- Once again, we don't know what operations a supports, so we can only return
-- either the first or second argument. Note that ex2' is the same as ex1.

--ex 3

ex3 :: Int -> a -> a
ex3 _ x = x
--ex3 = curry snd

-- We must return an a, and we have one: we don't know what we can do to it, so
-- we must just return it plain. (This is also the same as ex1.)

--ex 4

ex4 :: Bool -> a -> a -> a
ex4 True x _ = x
ex4 False _ y = y

-- Now we can do something more interesting: this is a function that can select
-- between its arguments based on the Bool passed in. We could do this the other
-- way around:

ex4' :: Bool -> a -> a -> a
ex4' False x _ = x
ex4' True _ y = y

-- Type algebra time!
-- Bool -> a -> a -> a = Bool -> (a -> a -> a) [property of functions]
--                     ~  2   ->       2       [# of inhabitants]
--                     = 2^2 = 4

-- So there are 4 functions that inhabit this type. The other two functions are
-- boring:

ex4boring :: Bool -> a -> a -> a
ex4boring _ x _ = x

ex4boring' :: Bool -> a -> a -> a
ex4boring' _ _ x = x


--ex 5

ex5 :: Bool -> Bool
ex5 = id

-- or

ex5' :: Bool -> Bool
ex5' = not

-- There are only two unary operations on Bool: the identity function, or
-- complement. So there are two (interesting) functions that inhabit this type.
-- There are also two boring ones: const True and const False.

-- So there are 4 functions that inhabit this type, a fact that we can also see
-- from type algebra:

-- number of inhabitants of Bool = 2
-- number of inhabitants of (a -> b) = b^a
-- number of inhabitants of (Bool -> Bool) = 2^2 = 4

-- For more on type algebra, see "The Algebra of Algebraic Data Types", a talk
-- from the London HUG. https://www.youtube.com/watch?v=YScIPA8RbVE

--ex 6

ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- This is not possible to write. We need to return an a, and the only way to
-- make one is to call the function (a -> a). But that requires that we have an
-- a to pass it. We have no way to make an a out of thin air.

--ex 7

ex7 :: (a -> a) -> a -> a
ex7 = ($)

-- or (much less interestingly)

ex7' :: (a -> a) -> a -> a
ex7' _ x = x

-- To make an a, we can either apply the function we're given to the a we're
-- given, or just return the a plain.

--ex 8

ex8 :: [a] -> [a]
ex8 = reverse

-- We have a list of a's. We don't know what we can do with a's, but we can
-- permute the list in some fashion, e.g. reverse it.

-- Of course, there are also the usual boring options:

ex8' :: [a] -> [a]
ex8' _ = []

ex8'' :: [a] -> [a]
ex8'' = id

--ex 9

ex9 :: (a -> b) -> [a] -> [b]
ex9 = map

-- We have a function from a to b, and a list of a's. The obvious thing to do is
-- call the function on each a to make a list of b's.

-- The trivial boring thing would be to just ignore the arguments and return an
-- empty list.

--ex 10

ex10 :: Maybe a -> a
ex10 = error "impossible"

-- We can't convert a Maybe a to an a, because what would we do with Nothing? We
-- can't make an a out of thin air, even if there exists a suitable sentinel
-- value of a for the use case.

--ex 11

ex11 :: a -> Maybe a
ex11 = Just

-- or

ex11' :: a -> Maybe a
ex11' = const Nothing

-- We can make a Maybe a out of the a that was passed in, or we can do the
-- boring thing.

--ex 12

ex12 :: Maybe a -> Maybe a
ex12 = id

-- or

ex12' :: Maybe a -> Maybe a
ex12' = const Nothing

-- We can either return the Maybe a we're given, or we can always return
-- Nothing. (We don't know what we can do to an a, so it's not useful to look at
-- a putative a inside a Just, and we can't create a Just a out of thin air.)

--ex 13

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST comp x (Node l v r)
  | comp x v == LT = Node (insertBST comp x l) v r
  | otherwise = Node l v (insertBST comp x r)

--ex 14

allCaps :: [String] -> Bool
allCaps = all isCapitalized

isCapitalized :: String -> Bool
isCapitalized [] = False
isCapitalized (c:_) = c == toUpper c

--ex 15

dropTrailingWhiteSpace :: String -> String
dropTrailingWhiteSpace = reverse . dropWhile isSpace . reverse

--ex 16

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

firstLetters :: [String] -> [Char]
firstLetters = mapMaybe safeHead

--ex 17

asList :: [String] -> String
asList l = '[' : intercalate "," l ++ "]"

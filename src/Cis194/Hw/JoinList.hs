module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
        | Single m a
        | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

jlToList :: (JoinList a b) -> [b]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append m l r) = (jlToList l) ++ (jlToList r)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (mappend (tag l) (tag r)) l r

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl
    | i < 0  = Nothing
    | i >= s = Nothing
    where s = getSize $ size $ tag jl
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Append _ l r)
    | i < ls = indexJ i l
    | otherwise = indexJ (i - ls) r
    where ls = getSize $ size $ tag l

dropJ :: (Sized b, Monoid b, Show b, Show a) => Int -> JoinList b a -> JoinList b a
dropJ i jl
    | i <= 0 = jl
    | i >= s = Empty
    where s = getSize $ size $ tag jl
dropJ _ Empty = Empty
dropJ i (Append _ l r)
    | i < ls    = let newL = dropJ i l
                  in Append (mappend (tag newL) (tag r)) newL r
    | otherwise = dropJ (i - ls) r
    where ls = getSize $ size $ tag l

takeJ :: (Sized b, Monoid b, Show b, Show a) => Int -> JoinList b a -> JoinList b a
takeJ i jl
    | i <= 0 = Empty
    | i >= s = jl
    where s = getSize $ size $ tag jl
takeJ _ Empty = Empty
takeJ _ s@(Single _ _) = s
takeJ i jl@(Append _ l r)
    | i < ls    = takeJ i l
    | otherwise = let newR = takeJ (i - ls) r
                  in Append (mappend (tag l) (tag newR)) l newR
    where ls = getSize $ size $ tag l

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid

import JoinList

import Scrabble
import Sized

import Buffer

s2jl :: String -> JoinList (Score, Size) String
s2jl x = Single (scoreString x, Size 1) x

s2jlb :: String -> JoinList (Score, Size) String
s2jlb = (foldr (\x a -> a +++ (s2jl x)) Empty) . lines

instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  fromString   = s2jlb
  line         = indexJ
  replaceLine n s b = (takeJ n b) +++ (s2jl s) +++ (dropJ (n + 1) b)
  numLines     = getSize . size . tag
  value        = getScore . fst . tag

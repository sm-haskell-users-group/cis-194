{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char

import JoinList
import Sized

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Sized Score where
  size (Score x) = Size x

getScore :: Score -> Int
getScore (Score x) = x

score :: Char -> Score
score c = snd $ head ((filter ((capC ==) . fst) letters) ++ [(' ', 0)])
    where capC = toUpper c
          letters = [
                        ('A',1),
                        ('B',3),
                        ('C',3),
                        ('D',2),
                        ('E',1),
                        ('F',4),
                        ('G',2),
                        ('H',4),
                        ('I',1),
                        ('J',8),
                        ('K',5),
                        ('L',1),
                        ('M',3),
                        ('N',1),
                        ('O',1),
                        ('P',3),
                        ('Q',10),
                        ('R',1),
                        ('S',1),
                        ('T',1),
                        ('U',1),
                        ('V',4),
                        ('W',4),
                        ('X',8),
                        ('Y',4),
                        ('Z',10)
                        ]


scoreString :: String -> Score
scoreString = sum . (map score)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Monoid Score where
    mempty = Score 0
    mappend (Score l) (Score r) = Score (l + r)
    mconcat l = Score $ sum $ map (\(Score x) -> x) l

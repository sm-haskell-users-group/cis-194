module Cis194.Hw.Week2 where

import Cis194.Hw.Words
import Data.List
import Data.Maybe (isJust)

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy word hand = func (sort word) (sort hand)
  where func "" _ = True
        func _ [] = False
        func (x:xs) (y:ys) =
          if x == y then func xs ys
          else if x > y then func (x:xs) ys
          else False

-- Came implemented already
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

-- I liked this one. Using `Maybe Hand` to represent success or failure seemed
-- reasonable, even in real-world scenarios; if all you have left is
-- `Just hand`, you can safely assume the word was successfully filled,
-- just place the word and don't even worry about the characters you dropped
-- on the floor.
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate tpl hand word = if length tpl /= length word then False else work
  where work = isJust $ foldr func (Just hand) (zip tpl word)
        func ('?', l) mh = mh >>= (\h -> (findIndex (== l) h) >>= (\i -> Just $ (take i h) ++ (drop (i + 1) h)))
        func (l1, l2) mh = if l1 == l2 then mh else Nothing

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate tpl hand = filter (wordFitsTemplate tpl hand) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord _ = 0

bestWords :: [String] -> [String]
bestWords _ = []

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate _ _ = 0

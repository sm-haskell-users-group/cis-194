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

-- I love it when functions come together like this.
scrabbleValueWord :: String -> Int
scrabbleValueWord = foldr ((+) . scrabbleValue) 0

-- Have to reverse this due to prepending instead of using `filter`. The
-- filter solution didn't really jump out at me, I need to give it more thought.
bestWords :: [String] -> [String]
bestWords = reverse . snd . foldr acc (0, [])
  where acc w (val, xs)
          | nextVal  > val = (nextVal, [w])
          | nextVal == val = (val, w : xs)
          | otherwise      = (val, xs)
            where nextVal = scrabbleValueWord w

-- It seemed so easy to pattern match on the multiplier character in this
-- way. I suspect there's a more elegant pattern, although probably not
-- without features in Haskell I don't know about yet.
--
-- Favorite part of this: `foldr id score mults`. I could have combined it
-- with the rest of the driving functions, but it was just too cool to obscure.
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate tpl word = foldr id score mults
  where (score, mults) = (foldr acc (0, [])) $ zip tpl word
        acc :: (Char, Char) -> (Int, [Int -> Int]) -> (Int, [Int -> Int])
        acc ('D', wc) (s, fns) = (s + 2 * scrabbleValue wc,        fns)
        acc ('T', wc) (s, fns) = (s + 3 * scrabbleValue wc,        fns)
        acc ('2', wc) (s, fns) = (s +     scrabbleValue wc, (*2) : fns)
        acc ('3', wc) (s, fns) = (s +     scrabbleValue wc, (*3) : fns)
        acc (_,   wc) (s, fns) = (s +     scrabbleValue wc,        fns)

module Cis194.Hw.Week2 where

import Words
import Data.List

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

-- type Word = String (or [Char]) ?!?
-- should these be newtype?

formableBy :: String -> Hand -> Bool
formableBy [] _     = True
formableBy (c:cs) h = elem c h && formableBy cs (delete c h)

formableBy' (c:s) h | elem c h = formableBy' s (delete c h) | True = False

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (flip formableBy hand) allWords

-- wild '?' equality
match :: Template -> String -> Bool
match [] [] = True
match (t:ts) (c:cs) = ('?' == t || t == c) && match ts cs
match _ _ = False

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate t h w = match t w && formableBy w (filter (/= '?') t ++ h)

wordFitsTemplate' template hand word
    -- does Haskell know this filter is loop invariant?
    | match template word = formableBy word (filter (/= '?') template ++ hand)
    | otherwise           = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h = filter (wordFitsTemplate t h) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . map scrabbleValue

scrabbleValueWord' word = sum (map scrabbleValue word)

type Score = Int -- letter(s) score

better :: (Score, [String]) -> String -> (Score, [String])
better (best, words) word | (score >  best) = (score, [word])
                          | (score == best) = (best, word : words) -- order matters!
                          | otherwise       = (best, words)
                          where score = scrabbleValueWord word

bestWords :: [String] -> [String]
bestWords words = snd (foldl better (0, []) words)

tFactors :: [(Char, (Int, Int))]
tFactors = zip ['2',    '3',       'D',    'T']
               [(2, 1), (3, 1), (1, 2), (1, 3)]

factoredScore :: Char -> Score -> (Int, Score)
factoredScore t score = case (lookup t tFactors) of
  Nothing                       -> (1,                     score)
  (Just (factor, letterFactor)) -> (factor, letterFactor * score)

-- "De?2?" "peace" == 24
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate st word = uncurry (*) $ foldl ƒ (1, 0) (zip st word) where
  ƒ :: (Int, Score) -> (Char, Char) -> (Int, Score)
  ƒ (f, s) (t, c) = (f * f', s + s') where (f', s') = factoredScore t (scrabbleValue c)

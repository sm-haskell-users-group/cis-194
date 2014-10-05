module Cis194.Hw.Week2 where

import Cis194.Hw.Words
import Data.List
import Data.Char

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
formableBy [] _ = True
formableBy (c:cs) h = (c `elem` h) && formableBy cs (delete c h)

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate ('?':xs) h (y:ys) = y `elem` h && wordFitsTemplate xs (delete y h) ys
wordFitsTemplate (x:xs) h (y:ys) = x == y && wordFitsTemplate xs h ys
wordFitsTemplate _ _ _ = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h = filter (wordFitsTemplate t h) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . map scrabbleValue

bestWords :: [String] -> [String]
bestWords = bestWords' []
  where bestWords' b [] = b
        bestWords' [] (w:ws) = bestWords' [w] ws
        bestWords' bs (w:ws) | valw > valb = bestWords' [w] ws
                             | valw == valb = bestWords' (w:bs) ws
                             | otherwise = bestWords' bs ws
          where valw = scrabbleValueWord w
                valb = scrabbleValueWord $ head bs

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate t s = n * sum (zipWith tilePlaceValue t s)
  where tilePlaceValue 'D' c = 2 * scrabbleValue c
        tilePlaceValue 'T' c = 3 * scrabbleValue c
        tilePlaceValue _ c = scrabbleValue c
        n = product $ map digitToInt $ filter (\c -> c == '2' || c == '3') t

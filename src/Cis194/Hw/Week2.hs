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

formableBy :: String -> Hand -> Bool
formableBy target hand = null $ foldl (dropOne) target hand
  where dropOne str char = case partition (==char) str of
                        (ys, zs) -> drop 1 ys ++ zs

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

-- Given a template in the form of "dayc?re" and a target-word
-- in the form "daycare", return a new string representing what
-- letters would be required in order to form the target-word.
--
-- Note that this function, in the event of a length mismatch
-- or misalignment of character (by index) across the two
-- strings, will result in a bang, e.g.:
--
-- template: "ca??" and word: "dash" -> "!sh"
-- 'c' and 'd' do not match; a '!' is inserted
-- 'a' and 'a' match and are thus ignored (not needed from hand)
-- '?' and 's' fit, so return 's'
-- '?' and 'h' fit, so return 'h'
converge :: Template -> String -> String
converge t s = converge' t s "" where
  converge' []        []       acc = acc
  converge' (t':ts')  []       acc = converge' ts' []  (acc ++ ['!'])
  converge' []        (s':ss') acc = converge' []  ss' (acc ++ ['!'])
  converge' ('?':ts') (s':ss') acc = converge' ts' ss' (acc ++ [s'])
  converge' (t':ts')  (s':ss') acc = converge' ts' ss' (if t' == s' then acc else acc ++ ['!'])

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate t h w = formableBy (converge t w) h

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h = filter (\w -> formableBy (converge t w) h) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord w = foldl (flip $ (+) . scrabbleValue) 0 w

bestWords :: [String] -> [String]
bestWords ws = bestWords' ws (0, []) where
  bestWords' [] (pts, acc) = acc
  bestWords' (w':ws') (pts, acc) = case compare (scrabbleValueWord w') pts of
                                   GT -> bestWords' ws' (scrabbleValueWord w', [w'])
                                   LT -> bestWords' ws' (pts, acc)
                                   EQ -> bestWords' ws' (pts, w':acc)

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate st w = scrabbleValueTemplate' st w (0, 1) where
  scrabbleValueTemplate' ('D':sts') (c':cs') (pts, mul) = scrabbleValueTemplate' sts' cs' (pts + (2 * scrabbleValue c'), mul)
  scrabbleValueTemplate' ('T':sts') (c':cs') (pts, mul) = scrabbleValueTemplate' sts' cs' (pts + (3 * scrabbleValue c'), mul)
  scrabbleValueTemplate' ('2':sts') (c':cs') (pts, mul) = scrabbleValueTemplate' sts' cs' (pts + scrabbleValue c', mul * 2)
  scrabbleValueTemplate' ('3':sts') (c':cs') (pts, mul) = scrabbleValueTemplate' sts' cs' (pts + scrabbleValue c', mul * 3)
  scrabbleValueTemplate' (_:sts')   (c':cs') (pts, mul) = scrabbleValueTemplate' sts' cs' (pts + scrabbleValue c', mul)
  scrabbleValueTemplate' _ _ (pts, mul) = pts * mul

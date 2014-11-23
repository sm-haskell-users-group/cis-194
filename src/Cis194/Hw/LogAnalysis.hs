module Cis194.Hw.LogAnalysis where

import Cis194.Hw.Log
import Data.List (sortBy)
import Data.Char (toLower)

-- My initial solution was much more verbose, including:
--   readInt' ts' >>= (\ts -> readInt' lvl' >>= \lvl -> Just (ts, lvl)
-- I learned from 601's solution using guards, and I think built upon it slightly
parseMessage :: String -> MaybeLogMessage
parseMessage = work . words
    where work ("I" :        ts' : xs) | (              ValidInt ts) <- (              readInt ts') = ValidLM $ LogMessage Info        ts $ unwords xs
          work ("W" :        ts' : xs) | (              ValidInt ts) <- (              readInt ts') = ValidLM $ LogMessage Warning     ts $ unwords xs
          work ("E" : lvl' : ts' : xs) | (ValidInt lvl, ValidInt ts) <- (readInt lvl', readInt ts') = ValidLM $ LogMessage (Error lvl) ts $ unwords xs
          work xs = InvalidLM $ unwords xs

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly lm = foldr accValid [] lm
    where accValid (ValidLM x) a = x : a
          accValid (InvalidLM _) a = a

parse :: String -> [LogMessage]
parse s = validMessagesOnly $ fmap parseMessage $ lines s

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ l _) (LogMessage _ r _) = compare l r

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ msg) = msg

testSeverity :: Int -> LogMessage -> Bool
testSeverity lvl (LogMessage (Error lvl') _ _) = lvl' >= lvl
testSeverity _ _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (fmap extractMessage) . (filter $ testSeverity 50) . sortMessages

-- No promises on performance here. I wonder if it would make sense to walk the
-- whole string, accumulating partial matches in a list, then succeed-fast when
-- any of them completes?
-- Thinking of something like isSubstr "aab" "aaaaab" that would produce:
--   ["a"], ["aa", "a"], ["aa", "a"], ["aa", "a"], ["aa", "a"], ["aab"]
-- Every partial would be checked at every iteration, so this would only be
-- good for very small partial matches
isSubstr :: String -> String -> Bool
isSubstr s = isSubstr' s
    where isSubstr' [] _ = True
          isSubstr' _ [] = False
          isSubstr' (x:xs) (s':str) | x == s' = if isSubstr' xs str then True
                                             else if (x:xs) == s then isSubstr' (x:xs) str
                                             else False
          isSubstr' xs _ | xs /= s = False
          isSubstr' xs (_:str) = isSubstr' xs str

messageContains :: String -> LogMessage -> Bool
messageContains s = (isSubstr $ lc s) . lc . extractMessage
    where lc = fmap toLower

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout s = filter (messageContains s)

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced s = (fmap extractMessage) . (filter $ (messageContains s) ||| (testSeverity 50)) . sortMessages

-- You get kudos if you can manage to use the following function:
(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x -- (||) is Haskell’s ordinary "or" operator

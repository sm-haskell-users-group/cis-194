module Cis194.Hw.LogAnalysis where

import Cis194.Hw.Log

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
compareMsgs _ _ = undefined

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages _ = undefined

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong _ = undefined

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout _ _ = undefined

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced _ _ = undefined

-- You get kudos if you can manage to use the following function:
(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x -- (||) is Haskell’s ordinary "or" operator

{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

parseMessage :: String -> LogMessage
parseMessage s
    | words s !! 0 == "E" = LogMessage (Error (read $ words s !! 1 :: Int)) (read $ words s !! 2 :: Int) (unwords . drop 3 $ words s)
    | words s !! 0 == "I" = LogMessage Info (read $ words s !! 1 :: Int) (unwords . drop 2 $ words s)
    | otherwise           = Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert l t
    | otherwise = t

build :: [LogMessage] -> MessageTree
build _ = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder _ = []

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong _ = []

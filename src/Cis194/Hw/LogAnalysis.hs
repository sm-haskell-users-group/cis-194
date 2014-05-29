{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc
import  Cis194.Hw.Log

extractInt :: [String] -> Int -> Int
extractInt xs i = read (xs !! i) :: Int

parseMessage :: String -> LogMessage

parseMessage ('W' : ' ' : line) = LogMessage Info timestamp message
    where splitLine = words line
          timestamp = extractInt splitLine 0
          message = unwords $ drop 1 splitLine

parseMessage ('I' : ' ' : line) = LogMessage Info timestamp message
    where splitLine = words line
          timestamp = extractInt splitLine 0
          message = unwords $ drop 1 splitLine

parseMessage ('E' : ' ' : line) = LogMessage (Error errorCode) timestamp message
    where splitLine = words line
          timestamp = extractInt splitLine 1
          errorCode = extractInt splitLine 0
          message = unwords $ drop 2 splitLine

parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

lm_ts :: LogMessage -> TimeStamp
lm_ts (LogMessage _ ts _) = ts
lm_ts (Unknown _) = -1

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf

insert m tree = if (lm_ts m) < (lm_ts center)
    then Node (insert m left) center right
    else Node left center (insert m right)
    where (Node left center right) = tree

build :: [LogMessage] -> MessageTree
build _ = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder _ = []

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong _ = []

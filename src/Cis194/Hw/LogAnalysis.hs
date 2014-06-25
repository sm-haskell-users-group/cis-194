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
insert (Unknown _) tree         = tree
insert m Leaf = Node Leaf m Leaf
insert m tree = if (lm_ts m) < (lm_ts center)
                    then Node (insert m left) center right
                    else Node left center (insert m right)
                    where (Node left center right) = tree

lm_ts :: LogMessage -> TimeStamp
lm_ts (LogMessage _ ts _) = ts
tm_ts (Unknown _) = -1

build :: [LogMessage] -> MessageTree
build messages = foldl (flip insert) Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map lm_m) . (filter isRelevant) . inOrder . build

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error severity) _ _) = severity >= 50
isRelevant _ = False

lm_m :: LogMessage -> String
lm_m (LogMessage _ _ m) = m
lm_m (Unknown s) = s

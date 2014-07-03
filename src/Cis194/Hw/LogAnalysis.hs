{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc
import  Cis194.Hw.Log

parseWords :: [String] -> LogMessage
parseWords ("I":t:ws) = (LogMessage (Info) (read t :: Int) (unwords ws))
parseWords ("W":t:ws) = (LogMessage (Warning) (read t :: Int) (unwords ws))
parseWords ("E":s:t:ws) = (LogMessage (Error (read s :: Int)) (read t :: Int) (unwords ws))
parseWords x = Unknown (unwords x)

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

severity :: MessageType -> Maybe Int
severity (Error i) = Just i
severity _ = Nothing

severe :: LogMessage -> Bool
severe (Unknown _) = False
severe m = ((maybe 0 id) (severity (messageType m))) >= 50

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m1 (Node t1 m2 t2)
    | (timeStamp m1) < (timeStamp m2) = (Node (insert m1 t1) m2 t2)
    | otherwise = (Node t1 m2 (insert m1 t2))

build :: [LogMessage] -> MessageTree
build [] = Leaf
build ms = insert (last ms) (build (init ms))

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 m t2) = (inOrder t1) ++ [m] ++ (inOrder t2)

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = (map message (inOrder (build (filter severe ms))))

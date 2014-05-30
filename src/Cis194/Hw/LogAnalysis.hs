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
insert l Leaf = Node Leaf l Leaf
insert l (Node left message right)
--    | isLogMessage && messageType == "(Error" = Node t l t -- divide the tree in left and right
--    | isLogMessage && messageType == "Info"   = Node t l t -- divide the tree in left and right
    | isLogMessage == False     = t
    | otherwise                 = Node left l right
    where isLogMessage          = words (show l) !! 0 == "LogMessage"
          messageType           = words (show l) !! 1
          timeStampErrorMessage = words (show l) !! 3
          timeStampInfoMessage  = words (show l) !! 2
          t                     = Node left message right

build :: [LogMessage] -> MessageTree
build _ = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder _ = []

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong _ = []

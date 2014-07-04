{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc
import  Cis194.Hw.Log

createLogMessage :: MessageType -> [String] -> LogMessage
createLogMessage _ [] = Unknown ""
createLogMessage typ (ts:message) =
  LogMessage typ (read ts) (unwords message)

parseMessage :: String -> LogMessage
parseMessage ('I':xs) = createLogMessage Info (words xs)
parseMessage ('W':xs) = createLogMessage Warning (words xs)
parseMessage ('E':xs) =
  createLogMessage (Error (read $ head $ words xs :: Int)) (tail $ words xs)
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message1 (Node left message2 right)
  | timeStamp message1 > timeStamp message2 =
    Node left message2 (insert message1 right)
  | timeStamp message1 <= timeStamp message2 =
    Node (insert message1 left) message2 right


build :: [LogMessage] -> MessageTree
build [] = Leaf
build x = foldl (flip insert) Leaf x

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) =
  (inOrder left) ++ [message] ++ (inOrder right)

isVital :: LogMessage -> Bool
isVital (LogMessage (Error n) _ _) = n > 50
isVital _ = False

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of thed
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong x = map messageString (inOrder $ build $ filter isVital x)

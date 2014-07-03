{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc
import  Cis194.Hw.Log

createLogMessage :: MessageType -> [String] -> LogMessage
createLogMessage _ [] = Unknown ""
createLogMessage typ (timeStamp:message) =
  LogMessage typ (read timeStamp) (unwords message)

parseMessage :: String -> LogMessage
parseMessage ('I':xs) = createLogMessage Info (words xs)
parseMessage ('W':xs) = createLogMessage Warning (words xs)
parseMessage ('E':xs) =
  createLogMessage (Error (read $ head $ words xs :: Int)) (tail $ words xs)
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

timestampFromMessage :: LogMessage -> Maybe TimeStamp
timestampFromMessage (Unknown _) = Nothing
timestampFromMessage (LogMessage _ x _) = Just x

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message1 (Node left message2 right)
  | timestampFromMessage message1 > timestampFromMessage message2 =
    Node left message2 (insert message1 right)
  | timestampFromMessage message1 <= timestampFromMessage message2 =
    Node (insert message1 left) message2 right


build :: [LogMessage] -> MessageTree
build [] = Leaf
build x = foldl (\tree message -> insert message tree) Leaf x

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf message Leaf) = [message]
inOrder (Node left message right) =
  (inOrder left) ++ [message] ++ (inOrder right)

returnMessageIfVital :: LogMessage -> String
returnMessageIfVital (LogMessage (Error n) _ message)
  | n >= 50 = message
  | otherwise = ""
returnMessageIfVital _ = ""

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of thed
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong x = filter (not . null) (map returnMessageIfVital (inOrder $ build x))

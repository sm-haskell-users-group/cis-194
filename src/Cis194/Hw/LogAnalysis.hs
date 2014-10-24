module LogAnalysis where

import Log
import Data.Char
import Data.List

--ex 1

parseMessage :: String -> MaybeLogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> MaybeLogMessage
parseMessage' lm@("E":err:sev:ws)
  = case readInt err of
     ValidInt e -> parseRest (Error e) (("E "++err):sev:ws)
     _ -> InvalidLM $ unwords lm
parseMessage' lm@("W":_) = parseRest Warning lm
parseMessage' lm@("I":_) = parseRest Info lm
parseMessage' lm = InvalidLM $ unwords lm

parseRest :: MessageType -> [String] -> MaybeLogMessage
parseRest mt lm@(_:ts:ws)
  = case readInt ts of
     ValidInt t -> ValidLM $ LogMessage mt t (unwords ws)
     _ -> InvalidLM $ unwords lm

--ex 2

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly = foldr accumLogMessages []
  where accumLogMessages (ValidLM lm) lms = lm:lms
        accumLogMessages _ lms = lms

--ex 3

parse :: String -> [LogMessage]
parse = validMessagesOnly . map parseMessage . lines

--ex 4

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _) = compare ts1 ts2

--ex 5

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

-- Alternatively, we could make LogMessage an instance of Ord

--ex 6

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage .
                sortMessages .
                filter isErrorGE50

isErrorGE50 :: LogMessage -> Bool
isErrorGE50 (LogMessage (Error s) _ _) = s >= 50
isErrorGE50 _ = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ m) = m

--ex 7

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout s = filter (messageAbout s)

messageAbout :: String -> LogMessage -> Bool
messageAbout s (LogMessage _ _ m) = toUpperStr s `isInfixOf` toUpperStr m
  where toUpperStr = map toUpper

--ex 8

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced s = map extractMessage .
                          sortMessages .
                          filter (isErrorGE50 ||| messageAbout s)

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) f g x = f x || g x -- (||) is Haskell's ordinary "or" operator

module Cis194.Hw.LogAnalysis where

import Cis194.Hw.Log

parseMessage :: String -> MaybeLogMessage
parseMessage _ = undefined

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly _ = undefined

parse :: String -> [LogMessage]
parse _ = undefined

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

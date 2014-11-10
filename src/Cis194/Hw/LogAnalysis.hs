{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}

module LogAnalysis where

import Log
import Data.List
import Data.Char

class Intish a where
  asInt :: a -> Int

instance Intish Char where
  asInt d = fromEnum d - fromEnum '0'

instance Intish [Char] where
  asInt _ = 600

splInt :: String -> (Maybe Int, String)
splInt (d:s) | elem d ['0'..'9'] = subseqInt (asInt d) s -- ['1'..'9']?
splInt s = (Nothing, s)

splInt2 :: String -> (Maybe Int, Maybe Int, String)
splInt2 s = (n', ts', m') where (n', s') = splInt s; (ts', m') = splInt s'

subseqInt :: Int -> String -> (Maybe Int, String)
subseqInt ß ( d :s) | elem d ['0'..'9'] = subseqInt (ß * 10 + asInt d) s
subseqInt ß (' ':s) = (Just ß,  s) -- ends with space, all good
subseqInt _      s  = (Nothing, s) -- invalid @s

parseMessage :: String -> MaybeLogMessage
parseMessage ('I':' ':s) | ((Just ts), m) <- splInt s = ValidLM $ LogMessage Info ts m
parseMessage ('W':' ':s) | ((Just ts), m) <- splInt s = ValidLM $ LogMessage Warning ts m
parseMessage ('E':' ':s) | ((Just severity),
                            (Just ts), m) <- splInt2 s = ValidLM $ LogMessage (Error severity) ts m
parseMessage s = InvalidLM s


validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
-- validMessagesOnly = foldr (\mlm -> case mlm of ValidLM lm -> (lm:); _ -> id) []
validMessagesOnly = foldl valid [] where
  valid ß (ValidLM lm) = lm:ß
  valid ß _ = ß

parse :: String -> [LogMessage]
parse = validMessagesOnly . map parseMessage . lines


compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ port _) (LogMessage _ star _) = compare port star

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

relevant :: LogMessage -> Bool
relevant (LogMessage (Error severity) _ _) = (severity >= 50)
relevant _ = False

message :: LogMessage -> String
message (LogMessage _ _ m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (message) . sortMessages . filter (relevant)

about :: String -> LogMessage -> Bool
about a (LogMessage _ _ m) = isInfixOf (map toLower a) (map toLower m)

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) f g x = f x || g x -- (||) is Haskell’s ordinary "or" operator

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced s = map message . sortMessages . filter (relevant ||| about s)

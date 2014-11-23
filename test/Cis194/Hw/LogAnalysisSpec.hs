module Cis194.Hw.LogAnalysisSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.LogAnalysis
import Cis194.Hw.Log

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "should parse an invalid log line" $ do
      let checkBad line = parseMessage line `shouldBe` (InvalidLM line)

      checkBad "invalid format"
      checkBad "E This line is missing the timestamp"
      checkBad "E 1234 This line is missing the info level"

    it "should parse valid info line" $ do
      parseMessage "I 1 msg" `shouldBe` (ValidLM $ LogMessage Info 1 "msg")

    it "should parse valid error line" $ do
      parseMessage "E 1 2 msg" `shouldBe` (ValidLM $ LogMessage (Error 1) 2 "msg")

    it "should parse valid warning line" $ do
      parseMessage "W 1 msg" `shouldBe` (ValidLM $ LogMessage Warning 1 "msg")

  describe "validMessagesOnly" $ do
    it "should filter out invalid messages" $ do
      let v1 = LogMessage Info 1 "msg"
      let v2 = LogMessage (Error 1) 2 "msg"
      let v3 = LogMessage Warning 3 "msg"

      let iv1 = InvalidLM "garbage"
      let iv2 = InvalidLM " E 1 2 msg"

      validMessagesOnly [ValidLM v1, iv1, ValidLM v2, iv2, ValidLM v3] `shouldBe` [v1, v2, v3]

  describe "parse" $ do
    it "should extract valid log messages from an input string" $ do
      let s = "I 1 msg1\n\
              \garbage\n\
              \E 10 2 msg2\n\
              \# problem line\n\
              \W 3 msg3"

      parse s `shouldBe` [
          LogMessage Info 1 "msg1",
          LogMessage (Error 10) 2 "msg2",
          LogMessage Warning 3 "msg3"
        ]

  describe "compareMsgs" $ do
    it "should correctly order messages by timestamp" $ do
      let v1 = LogMessage Info 1 "msg1"
      let v2 = LogMessage Warning 1 "msg3"
      let v3 = LogMessage (Error 10) 2 "msg2"

      compareMsgs v1 v3 `shouldBe` LT
      compareMsgs v3 v1 `shouldBe` GT
      compareMsgs v1 v1 `shouldBe` EQ
      compareMsgs v1 v2 `shouldBe` EQ

  describe "sortMessages" $ do
    it "should correctly order a list of LogMessages" $ do
      let v1 = LogMessage Info 1 "msg1"
      let v2 = LogMessage Warning 1 "msg3"
      let v3 = LogMessage (Error 10) 2 "msg2"

      sortMessages [v1, v3, v2] `shouldBe` [v1, v2, v3]

  describe "whatWentWrong" $ do
    it "should filter and extract messages from severe messages" $ do
      let v1 = LogMessage Info 1 "msg1"
      let v2 = LogMessage (Error 100) 1 "msg2"
      let v3 = LogMessage Warning 2 "msg3"
      let v4 = LogMessage (Error 80) 3 "msg4"

      whatWentWrong [v1, v4, v3, v2] `shouldBe` ["msg2", "msg4"]

  describe "messagesAbout" $ do
    it "should return all log messages about a phrase" $ do
      let v1 = LogMessage Info 1 "blah foo zzz"
      let v2 = LogMessage (Error 100) 1 "bar"
      let v3 = LogMessage Warning 2 "1 foo 3"
      let v4 = LogMessage (Error 80) 3 "baz"

      messagesAbout "foo" [v1, v2, v3, v4] `shouldBe` [v1, v3]
      messagesAbout "blah" [LogMessage Info 1 "balaaaha"] `shouldBe` []

  describe "isSubstr" $ do
    it "should detect substrings" $ do
      isSubstr "abc" "aaaabcccccc" `shouldBe` True

    it "should return False on no match" $ do
      isSubstr "blah" "foo" `shouldBe` False

    it "should reset after false matches" $ do
      isSubstr "abc" "a1b1c" `shouldBe` False

    it "should fail if matching against a shorter string" $ do
      isSubstr "abc" "ab" `shouldBe` False

  describe "whatWentWrongEnhanced" $ do
    it "should show high severity logs /and/ lines that match a string" $ do
      let v1 = LogMessage Info 1 "blah"
      let v2 = LogMessage (Error 100) 1 "bar"
      let v3 = LogMessage Warning 2 "1 foo 3"
      let v4 = LogMessage (Error 80) 3 "baz"

      whatWentWrongEnhanced "foo" [v1, v2, v3, v4] `shouldBe` ["bar", "1 foo 3", "baz"]

module Cis194.Hw.Week5Spec (main, spec) where

import Test.Hspec
import Cis194.Hw.Week5
import Ring
import Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "int parsing" $ do
    it "should parse an integer out of a string" $ do
      parse "3" `shouldBe` Just (3 :: Integer, "")

    it "should parse a ring out of a string" $ do
      parseRing "1 + 2 * 5" `shouldBe` Just (11 :: Integer)

    it "should return 0 for the additive identity" $ do
      addId `shouldBe` (0 :: Integer)
  describe "Mod5 stuff" $ do
    it "should parse a Mod5 integer out of a string" $ do
      parse "7" `shouldBe` Just (Mod5 5, "")

module Cis194.Hw.Week5Spec where

import Test.Hspec

import Cis194.Hw.Parser
import Cis194.Hw.Ring
import Cis194.Hw.Week5

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "intParsingWorks" $ do
    it "should be able to parse plain integers" $ do
      parse "3" `shouldBe` Just (3 :: Integer, "")

    it "should be able to parse an Integer ring" $ do
      parseRing "1 + 2 * 5" `shouldBe` Just (11 :: Integer)

    it "should know the additive identity" $ do
      addId `shouldBe` (0 :: Integer)

  describe "Ring Mod5" $ do
    it "should be able to parse plain integers" $ do
      parse "3" `shouldBe` Just (MkMod 3, "")

    it "should be able to parse a Mod5 ring" $ do
      parseRing "1 + 2 * 5" `shouldBe` Just (MkMod 1)

    it "should deal with negatives" $ do
      parseRing "1 + (-2)" `shouldBe` Just (MkMod 4)

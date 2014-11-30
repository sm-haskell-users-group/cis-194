{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Cis194.Hw.Week6Spec where

import Cis194.Hw.Week6

import Data.Aeson
import Test.Hspec

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "aeson" $ do
    it "should parse strings" $ do
      eitherDecode ("[5,4,3,2,1]" :: B.ByteString) `shouldBe` (Right [5,4,3,2,1] :: Either String [Integer])

  describe "week6" $ do
    it "should convert \"Y\"/\"N\" values to True/False" $ do
      ynToBool (String "Y") `shouldBe` (Bool True)
      ynToBool (String "N") `shouldBe` (Bool False)
      ynToBool (String "other") `shouldBe` (String "other")

    it "should parse data" $ do
      dat <- B.readFile "data/markets.json"
      let (Right (Array arr)) = parseData dat
      V.length arr `shouldBe` 8144

    it "should parse values that include \"Y\" and \"N\"" $ do
      parseData "[\"Y\", \"N\", \"Y\", \"N\"]" `shouldBe` Right (Array $ V.fromList [Bool True, Bool False, Bool True, Bool False])

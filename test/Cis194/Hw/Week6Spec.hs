{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Cis194.Hw.Week6Spec where

import Cis194.Hw.Week6

import Data.Aeson
import Data.Monoid
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

    it "should parse markets" $ do
      dat <- B.readFile "data/markets.json"
      let (Right markets) = parseMarkets dat
      length markets `shouldBe` 8144

    it "should load data directly" $ do
      mkts <- loadData
      length mkts `shouldBe` 8144

    it "should search for markets by name" $ do
      mkts <- loadData
      (length $ search (:[]) "Brandywine" mkts) `shouldBe` 1
      (length $ search (:[]) "Madison" mkts) `shouldBe` 19

  describe "OrdList" $ do
    it "should maintain order" $ do
      let evens = OrdList [2,4,6] :: OrdList Integer
      let odds = OrdList [1,3,5] :: OrdList Integer
      let combined = evens <> odds

      combined `shouldBe` OrdList [1,2,3,4,5,6]

  describe "firstFound" $ do
    it "should find markets" $ do
      mkts <- loadData
      (fmap fmid $ firstFound "Madison" mkts) `shouldBe` Just 1002454

    it "should not find markets that don't exist" $ do
      mkts <- loadData
      (fmap fmid $ firstFound "blahblahblah" mkts) `shouldBe` Nothing

  describe "lastFound" $ do
    it "should find markets" $ do
      mkts <- loadData
      (fmap fmid $ lastFound "Madison" mkts) `shouldBe` Just 1005967

    it "should not find markets that don't exist" $ do
      mkts <- loadData
      (fmap fmid $ lastFound "blahblahblah" mkts) `shouldBe` Nothing

  describe "allFound" $ do
    it "should find markets" $ do
      mkts <- loadData
      (fmap fmid $ allFound "Madison" mkts) `shouldBe` [1002454, 1006490, 1006501, 1001855, 1000481, 1002419, 1001120, 1003296, 1005504, 1000117, 1002705, 1001636, 20206, 1005201, 1000562, 1007520, 1007290, 1007291, 1005967]

    it "should not find markets that don't exist" $ do
      mkts <- loadData
      (fmap fmid $ allFound "blahblahblah" mkts) `shouldBe` []

  describe "numberFound" $ do
    it "should count markets" $ do
      mkts <- loadData
      numberFound "Madison" mkts `shouldBe` 19

    it "should return 0 for nonexistent markets" $ do
      mkts <- loadData
      numberFound "blahblahblah" mkts `shouldBe` 0

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Cis194.Hw.Week6Spec where

import Data.Aeson
import Test.Hspec

import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "aeson" $ do
    it "should parse strings" $ do
      eitherDecode ("[5,4,3,2,1]" :: B.ByteString) `shouldBe` (Right [5,4,3,2,1] :: Either String [Integer])

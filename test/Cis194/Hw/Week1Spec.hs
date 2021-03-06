module Cis194.Hw.Week1Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cis194.Hw.Week1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "should split digits of integer into a list" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]

    it "should return an empty list for zero" $ do
      toDigits 0 `shouldBe` []

    -- uses QuickCheck - need to learn how to generate constrained inputs
    it "should return an empty list for negative numbers" $ property $
      \x -> (if x < 0 then (toDigits x) else []) == ([] :: [Integer])

  describe "doubleEveryOther" $ do
    it "should return an empty list given an empty list" $ do
      doubleEveryOther [] `shouldBe` []

    it "should double every other int in the list, from right to left" $ do
      doubleEveryOther [8,7,6,5] `shouldBe`[16,7,12,5]
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

  describe "sumDigits" $ do
    it "should return zero for an empty list" $ do
      sumDigits [] `shouldBe` 0

    it "should sum all digits in the list" $ do
      sumDigits [16,7,12,5] `shouldBe` 22
      sumDigits [18,7,33,5] `shouldBe` 27

  describe "validate" $ do
    it "should return True for valid card number" $ do
      validate 4012888888881881 `shouldBe` True

    it "should return False for invalid card number" $ do
      validate 4012888888881882 `shouldBe` False

  describe "hanoi" $ do
    it "should return an empty list of moves if no disks" $ do
      hanoi 0 "a" "b" "c" `shouldBe` []

    it "should return a single move to go from first to second peg if only one disk" $ do
      hanoi 1 "a" "b" "c" `shouldBe` [("a", "b")]

    it "should return a list of moves required to take all 2 disks from peg a to c using b as the intermediate" $ do
      hanoi 2 "a" "c" "b" `shouldBe` [("a","b"), ("a","c"), ("b","c")]

    it "should return a list of moves required to take all 3 disks from peg a to c using b as the intermediate" $ do
      hanoi 3 "a" "c" "b" `shouldBe` [("a","c"), ("a","b"), ("c","b"), ("a","c"), ("b","a"), ("b","c"), ("a","c")]

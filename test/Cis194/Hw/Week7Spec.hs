module Cis194.Hw.Week7Spec where

import System.Random (mkStdGen)
import Test.Hspec

import Cis194.Hw.Week7

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "should find the correct number in the Fibonacci series" $ do
      fib 5 `shouldBe` 5

  describe "fibs1" $ do
    it "should represent the infinite series of Fibonacci numbers" $ do
      take 5 fibs1 `shouldBe` [0, 1, 1, 2, 3]

  describe "fibs2" $ do
    it "should represent the infinite series of Fibonacci numbers" $ do
      take 5 fibs2 `shouldBe` [0, 1, 1, 2, 3]

  describe "streamToList" $ do
    it "should convert a stream into a list" $ do
      let stream = Cons 0 (Cons 1 stream) :: Stream Integer
      take 5 (streamToList stream) `shouldBe` [0, 1, 0, 1, 0]

  describe "Show (Stream a)" $ do
    it "should show the first 20 elements in a Stream" $ do
      let stream = Cons 0 (Cons 1 stream) :: Stream Integer
      (show stream) `shouldBe` "Stream(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, ...)"

  describe "streamRepeat" $ do
    it "should generate a stream of the repeated value" $ do
      let stream = streamRepeat 9 :: Stream Integer
      take 5 (streamToList stream) `shouldBe` [9, 9, 9, 9, 9]

  describe "streamMap" $ do
    it "should map a function across a stream" $ do
      let stream = streamRepeat 9 :: Stream Integer
      take 5 (streamToList $ streamMap (\x -> x - 4) stream) `shouldBe` [5, 5, 5, 5, 5]

  describe "streamFromSeed" $ do
    it "should be able to build a stream from a seed" $ do
      let stream = streamFromSeed (*2) 1 :: Stream Integer
      take 5 (streamToList stream) `shouldBe` [1, 2, 4, 8, 16]

    it "should be able to work on strings as well" $ do
      take 5 (streamToList (streamFromSeed ('x' :) "o")) == ["o", "xo", "xxo", "xxxo", "xxxxo"]

  describe "nats" $ do
    it "should describe all natural numbers, starting at 0" $ do
      take 5 (streamToList nats) `shouldBe` [0, 1, 2, 3, 4]

  describe "ruler" $ do
    it "should describe all numbers in the ruler series" $ do
      take 10 (streamToList ruler) `shouldBe` [0, 0, 1, 0, 2, 0, 1, 0, 3, 0]

  describe "randomList" $ do
    it "should generate the same list given the same seed" $ do
      let randIntList = randomList . mkStdGen :: Int -> [Int]
      (take 20 $ randIntList 1) `shouldBe` (take 20 $ randIntList 1)

    it "should generate different lists given different seeds" $ do
      let randIntList = randomList . mkStdGen :: Int -> [Int]
      (take 20 $ randIntList 1) `shouldSatisfy` (/= (take 20 $ randIntList 2))

  describe "randomInts" $ do
    it "should produce a pseudo-random list of ints of a specified length" $ do
      length (randomInts 5) `shouldBe` 5

  describe "fib4" $ do
    it "should figure out the correct number in the sequence" $ do
      fib4 10 `shouldBe` 55

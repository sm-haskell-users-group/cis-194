module Cis194.Hw.Week2Spec (main, spec) where

import Test.Hspec
import Cis194.Hw.Week2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formableBy" $ do
    it "should tell you if a word is formable by tiles in a hand" $ do
      formableBy "fun" ['x','n','i','f','u','e','l'] `shouldBe` True
      formableBy "haskell" ['k','l','e','h','a','l','s'] `shouldBe` True
      formableBy "haskell" ['k','l','e','h','a','y','s'] `shouldBe` False

  describe "wordsFrom" $ do
    it "should give a list of all words formable by a hand" $ do
      wordsFrom ['a','b','c','d'] `shouldBe` ["ab","ad","ba","bad","cab","cad","dab"]

      wordsFrom ['h','e','l','l','o'] `shouldBe`
        [ "eh","el","ell","he","hell","hello","helo"
        , "ho","hoe","hole","lo","oe","oh","ole" ]

  describe "wordFitsTemplate" $ do
    it "checks to see if a word matches a template, given a set of tiles" $ do
      wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" `shouldBe` True
      wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care" `shouldBe` False
      wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" `shouldBe` False
      wordFitsTemplate "let" ['x','x'] "let" `shouldBe` True

  describe "wordsFittingTemplate" $ do
    it "produces all words that match a given template" $ do
      wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'] `shouldBe`
        ["acre","bare","carb","care","carl","earl"]

  describe "scrabbleValueWord" $ do
    it "gives the value of a word" $ do
      scrabbleValueWord "care" `shouldBe` 6
      scrabbleValueWord "quiz" `shouldBe` 22

  describe "bestWords" $ do
    it "returns the highest-scoring words" $ do
      bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']) `shouldBe` ["carb"]
      bestWords ["cat", "rat", "bat"] `shouldBe` ["bat","cat"]
      bestWords [] `shouldBe` []

  describe "scrabbleValueTemplate" $ do
    it "computes the value of a given word on a given template" $ do
      scrabbleValueTemplate "?e??3" "peace" `shouldBe` 27
      scrabbleValueTemplate "De?2?" "peace" `shouldBe` 24
      scrabbleValueTemplate "??Tce" "peace" `shouldBe` 11

-- Extra Credit
  describe "wordFitsTemplate'" $ do
    it "checks to see if a word matches an arbitrarily-lengthed template, given a set of tiles" $ do
      wordFitsTemplate' "??r???" ['c','x','e','a','b','c','l'] "care" `shouldBe` True
      wordFitsTemplate' "??r"    ['c','x','e','a','b','c','l'] "care" `shouldBe` False
      wordFitsTemplate' "??r?"   ['c','x','e','w','b','c','l'] "care" `shouldBe` False
      wordFitsTemplate' "??r?"   ['c','x','e','a','b','c','l'] "car" `shouldBe` True
      wordFitsTemplate' "let"    ['x','x'] "let" `shouldBe` True

module Cis194.Hw.CalcSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.Calc
import Cis194.Hw.ExprT
import Cis194.Hw.JoinList

import Data.Monoid

main :: IO ()
main = hspec spec

type EditorList = JoinList (Product Int) Char

spec :: Spec
spec = do
  describe "JoinList" $ do
    it "should handle empty" $ do
      (Empty :: EditorList) `shouldBe` Empty

    it "should support combining structures" $ do
      (Single (Product 2) 'a') +++ (Single (Product 3) 'b')
      `shouldBe`
      (Append (Product 6) (Single (Product 2) 'a') (Single (Product 3) 'b'))

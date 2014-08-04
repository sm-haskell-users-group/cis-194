module Cis194.Hw.CalcSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.Calc
import Cis194.Hw.ExprT
import Cis194.Hw.JoinList
import Sized

import Data.Monoid

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

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

    it "should support indexing" $ do
      let jl = Append (Size 3)
                   (Append (Size 2)
                     (Single (Size 1) "y")
                     (Single (Size 1) "ea"))
                   (Single (Size 1) "h")
      let t = (\i -> (indexJ i jl) `shouldBe` (jlToList jl !!? i))

      t 0
      t 1
      t 2
      t 3
      t 4

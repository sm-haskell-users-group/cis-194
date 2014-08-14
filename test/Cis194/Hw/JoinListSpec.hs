module Cis194.Hw.CalcSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.Calc
import Cis194.Hw.ExprT
import Cis194.Hw.JoinList
import Sized
import Scrabble

import Data.Monoid
import Debug.Trace

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
  let jl1 = Append (Size 3)
               (Append (Size 2)
                 (Single (Size 1) "y")
                 (Single (Size 1) "ea"))
               (Single (Size 1) "h")

  let jl2 = Single (Size 1) "hello"

  describe "JoinList" $ do
    it "should handle empty" $ do
      (Empty :: EditorList) `shouldBe` Empty

    it "should support combining structures" $ do
      (Single (Product 2) 'a') +++ (Single (Product 3) 'b')
      `shouldBe`
      (Append (Product 6) (Single (Product 2) 'a') (Single (Product 3) 'b'))

    it "should support indexing" $ do
      let t = \jl i -> let lhs = (indexJ i jl)
                           rhs = (jlToList jl !!? i)
                       in lhs `shouldBe` rhs
--                       in (trace (show (i, lhs, rhs)) lhs `shouldBe` rhs)

      t jl1 0
      t jl1 1
      t jl1 2
      t jl1 3
      t jl1 4

      t jl2 0
      t jl2 1

    it "should support dropping" $ do
      let t = \jl i -> let lhs = (jlToList $ dropJ i jl)
                           rhs = (drop i $ jlToList jl)
                       in lhs `shouldBe` rhs
--                       in (trace (show (i, lhs, rhs)) lhs `shouldBe` rhs)
      t jl1 (-1)
      t jl1 0
      t jl1 1
      t jl1 2
      t jl1 3
      t jl1 4

      t jl2 0
      t jl2 1

    it "should support taking" $ do
      let t = \jl i -> let lhs = (jlToList $ takeJ i jl)
                           rhs = (take i $ jlToList jl)
                       in lhs `shouldBe` rhs
--                       in trace (show (i, lhs, rhs)) lhs `shouldBe` rhs

      t jl1 (-1)
      t jl1 0
      t jl1 1
      t jl1 2
      t jl1 3
      t jl1 4

      t jl2 0
      t jl2 1

    it "should play Scrabble" $ do
      let scr2 = Append (Score 23)
                   (Single (Score 9) "yay ")
                   (Single (Score 14) "haskell!")

      let t1 = scoreLine "yay " +++ scoreLine "haskell!"

      t1 `shouldBe` scr2

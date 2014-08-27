module Cis194.Hw.PartySpec (main, spec) where

import Test.Hspec
import Data.Monoid

import Cis194.Hw.Employee
import Cis194.Hw.Party

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "GuestList" $ do
    it "should store Employees" $ do
      let emp = Emp "Fred" 10
      GL [emp] 10 `shouldBe` GL [emp] 10

    it "should accept new employees" $ do
      let emp = Emp "Fred" 10
      glCons emp (GL [] 0) `shouldBe` (GL [emp] 10)

    it "should implement Monoid" $ do
      let emp = Emp "Fred" 10
      let l = glCons emp mempty
      mappend l l `shouldBe` GL [emp, emp] 20
      mappend l l `shouldBe` mconcat [l, l]

    it "should support comparison" $ do
      let emp1 = Emp "Fred" 10
      let emp2 = Emp "Judy" 15

      let list1 = glCons emp2 $ glCons emp1 mempty
      let list2 = glCons emp1 mempty

      moreFun list1 list2 `shouldBe` list1

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

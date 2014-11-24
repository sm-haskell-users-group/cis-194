{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cis194.Hw.Week4Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative

import Cis194.Hw.BST
import Cis194.Hw.Week4

-- TESTING CODE. (Students aren't expected to understand this yet, but it
-- might be interesting to read, anyway!)

instance Arbitrary a => Arbitrary (BST a) where
  arbitrary = sized mk_tree

mk_tree :: Arbitrary a => Int -> Gen (BST a)
mk_tree 0 = return Leaf
mk_tree n = frequency [ (1, return Leaf)
                      , (2, Node <$> mk_tree (n `div` 2)
                                 <*> arbitrary
                                 <*> mk_tree (n `div` 2)) ]

prop_ordered :: BST Int -> Bool
prop_ordered x = isBST compare x == is_sorted (getElements x)
  where
    is_sorted []             = True
    is_sorted [_]            = True
    is_sorted (x1 : x2 : xs) = x1 <= x2 && is_sorted (x2 : xs)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BST" $ do
    it "should be ordered" $ property $
      prop_ordered

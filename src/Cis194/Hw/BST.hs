{- BST.hs
   Part of an implementation of a binary search tree.
-}

module Cis194.Hw.BST where

data BST a = Leaf
           | Node (BST a) a (BST a)
  deriving (Show, Eq)

-- | Is the tree empty?
isEmpty :: BST a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Is the tree a BST between the given endpoints?
isBSTBetween :: (a -> a -> Ordering)    -- ^ comparison function
             -> Maybe a                 -- ^ lower bound, if one exists
             -> Maybe a                 -- ^ upper bound, if one exists
             -> BST a                   -- ^ tree to test
             -> Bool
isBSTBetween _   _       _       Leaf = True
isBSTBetween cmp m_lower m_upper (Node left x right)
  = isBSTBetween cmp m_lower  (Just x) left  &&
    isBSTBetween cmp (Just x) m_upper  right &&
    case m_lower of
      Just lower -> lower `cmp` x /= GT
      Nothing    -> True
    &&
    case m_upper of
      Just upper -> x `cmp` upper /= GT
      Nothing    -> True

-- | Is this a valid BST?
isBST :: (a -> a -> Ordering) -> BST a -> Bool
isBST cmp = isBSTBetween cmp Nothing Nothing

-- | Get a list of the elements in sorted order
getElements :: BST a -> [a]
getElements Leaf                = []
getElements (Node left x right) = getElements left ++ x : getElements right

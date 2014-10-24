module Week5 where

import Ring
import Parser
import Data.List
import Data.Maybe

--ex 1

-- every definition has some test definitions to show that it works

--ex 2

data Mod5 = MkMod Integer
          deriving (Show, Read, Eq)

instance Ring Mod5 where
  addId = MkMod 0
  addInv (MkMod n) = MkMod (5 - n)
  mulId = MkMod 1

  add (MkMod n) (MkMod m) = MkMod $ (n + m) `mod` 5
  mul (MkMod n) (MkMod m) = MkMod $ (n * m) `mod` 5

ringMod5Works :: Bool
ringMod5Works = add (MkMod 3) (MkMod 3) == MkMod 1
            && mul (MkMod 3) (MkMod 3) == MkMod 4
            && add (MkMod 1) (addInv (MkMod 1)) == addId
            && mul (MkMod 3) mulId == MkMod 3

instance Parsable Mod5 where
  parse s = do
    r <- stripPrefix "MkMod" s
    (i, rest) <- parse r :: Maybe (Integer, String)
    return (MkMod i, rest)

parseMod5Works :: Bool
parseMod5Works = (parse "MkMod 2" == Just (MkMod 2, ""))
                 && isNothing (parse "nowt" :: Maybe (Mod5, String))

--ex 3

data Mat2x2 = MkMat Integer Integer Integer Integer
            deriving Eq

instance Show Mat2x2 where
  show (MkMat a b c d) = "[[" ++ show a ++ "," ++ show b ++ "]["
                         ++ show c ++ "," ++ show d ++ "]]"

instance Ring Mat2x2 where
  addId = MkMat 0 0 0 0
  addInv (MkMat a b c d) = MkMat (negate a) (negate b) (negate c) (negate d)
  mulId = MkMat 1 0 0 1

  add (MkMat a b c d) (MkMat e f g h) = MkMat (a+e) (b+f) (c+g) (d+h)
  mul (MkMat a b c d) (MkMat e f g h) = MkMat (a*e + b*g) (a*f + b*h)
                                        (c*e + d*g) (c*f + d*h)
ringMatWorks :: Bool
ringMatWorks = add (MkMat 1 1 1 1) (MkMat 2 2 2 2) == MkMat 3 3 3 3
               && mul (MkMat 1 2 3 4) (MkMat 1 2 3 4) == MkMat 7 10 15 22
               && add (MkMat 1 2 3 4) (addInv (MkMat 1 2 3 4)) == addId
               && mul (MkMat 1 2 3 4) mulId == MkMat 1 2 3 4

instance Parsable Mat2x2 where
  parse s = do
    r <- stripPrefix "[[" s
    (a, r) <- parse r :: Maybe (Integer, String)
    r <- stripPrefix "," r
    (b, r) <- parse r :: Maybe (Integer, String)
    r <- stripPrefix "][" r
    (c, r) <- parse r :: Maybe (Integer, String)
    r <- stripPrefix "," r
    (d, r) <- parse r :: Maybe (Integer, String)
    r <- stripPrefix "]]" r
    return (MkMat a b c d, r)

ringParseWorks :: Bool
ringParseWorks = parse "[[1,2][3,4]]" == Just (MkMat 1 2 3 4, "")
                 && isNothing (parse "nowt" :: Maybe (Mat2x2, String))

--ex 4

instance Ring Bool where
  addId = False
  addInv = id
  mulId = True

  add = (/=)
  mul = (&&)

ringBoolWorks :: Bool
ringBoolWorks = add True False
                && not (mul True False)
                && add True (addInv True) == addId
                && mul True mulId

instance Parsable Bool where
  parse = listToMaybe . reads

boolParseWorks :: Bool
boolParseWorks = parse "True" == Just (True, "")
                 && parse "False" == Just (False, "")
                 && isNothing (parse "nowt" :: Maybe (Bool, String))

--ex 5

distribute :: RingExpr a -> RingExpr a
distribute (Mul (Add x y) z) = Add (Mul x' z') (Mul x' y')
  where x' = distribute x
        y' = distribute y
        z' = distribute z
distribute (Mul x (Add y z)) = distribute (Mul (Add x y) z) -- Add is commutative
distribute (Add x y) = Add (distribute x) (distribute y)
distribute (AddInv x) = AddInv (distribute x)
distribute e = e

distributeWorks :: Bool
distributeWorks = fmap distribute (parseRing "(1 + 2) * 3" :: Maybe (RingExpr Integer))
                  == Just (Add (Mul (Lit 1) (Lit 3)) (Mul (Lit 1) (Lit 2)))
                  &&
                  fmap distribute (parseRing "3 * (1 + 2)" :: Maybe (RingExpr Integer))
                  == Just (Add (Mul (Lit 3) (Lit 2)) (Mul (Lit 3) (Lit 1)))

--ex 6

-- there is an error in the notes here: for RingExpr Integer, the multiplicative
-- identity is Lit 1, not Lit 0

-- first convert Literals to MulIds
litToMulId :: (Ring a, Eq a) => RingExpr a -> RingExpr a
litToMulId (Mul x y) = Mul (litToMulId x) (litToMulId y)
litToMulId (Add x y) = Add (litToMulId x) (litToMulId y)
litToMulId (AddInv x) = AddInv (litToMulId x)
litToMulId (Lit x)
  | x == mulId = MulId
  | otherwise = Lit x

-- then squash the MulIds
squashMulId :: (Ring a, Eq a) => RingExpr a -> RingExpr a
squashMulId = squashMulId' . litToMulId

squashMulId' :: RingExpr a -> RingExpr a
squashMulId' (Mul MulId x) = squashMulId' x
squashMulId' (Mul x MulId) = squashMulId' x
squashMulId' (Add x y) = Add (squashMulId' x) (squashMulId' y)
squashMulId' (AddInv x) = AddInv (squashMulId' x)
squashMulId' e = e

squashMulIdWorks :: Bool
squashMulIdWorks = fmap squashMulId (parseRing "(2 + 2) * 1" :: Maybe (RingExpr Integer))
                   == Just (Add (Lit 2) (Lit 2))
                   &&
                   fmap squashMulId (parseRing "1 * (2 + 2)" :: Maybe (RingExpr Integer))
                   == Just (Add (Lit 2) (Lit 2))

--ex 7

-- we need a function that will traverse an expression and apply a
-- transformation at each stage

transformExpression :: (RingExpr a -> RingExpr a) -> RingExpr a -> RingExpr a
transformExpression f (Mul x y) = f (Mul (f x) (f y))
transformExpression f (Add x y) = f (Add (f x) (f y))
transformExpression f (AddInv x) = f (AddInv (f x))
transformExpression f e = f e

-- distribute (just expressing what we want to work on)
distribute' :: RingExpr a -> RingExpr a
distribute' (Mul (Add x y) z) = Add (Mul x z) (Mul x y)
distribute' (Mul x (Add y z)) = distribute' (Mul (Add x y) z)
distribute' e = e

newDistribute :: RingExpr a -> RingExpr a
newDistribute = transformExpression distribute'

newDistributeWorks :: Bool
newDistributeWorks = fmap newDistribute (parseRing "(1 + 2) * 3" :: Maybe (RingExpr Integer))
                     == fmap distribute (parseRing "(1 + 2) * 3" :: Maybe (RingExpr Integer))
                     &&
                     fmap newDistribute (parseRing "3 * (1 + 2)" :: Maybe (RingExpr Integer))
                     == fmap distribute (parseRing "3 * (1 + 2)" :: Maybe (RingExpr Integer))

-- convert literals
litsToIds :: (Ring a, Eq a) => RingExpr a -> RingExpr a
litsToIds (Lit x)
  | x == mulId = MulId
--  | x == addId = AddId
  | otherwise = Lit x
litsToIds e = e

-- squash literals (just expressing what we want to work on)
squashIds :: RingExpr a -> RingExpr a
squashIds (Mul MulId x) = x
squashIds (Mul x MulId) = x
--squashIds (Add AddId x) = x
--squashIds (Add x AddId) = x
squashIds e = e

newSquashIds :: (Ring a, Eq a) => RingExpr a -> RingExpr a
newSquashIds = transformExpression (squashIds . litsToIds)

newSquashIdsWorks :: Bool
newSquashIdsWorks = fmap squashMulId (parseRing "(2 + 2) * 1" :: Maybe (RingExpr Integer))
                    == fmap newSquashIds (parseRing "(2 + 2) * 1" :: Maybe (RingExpr Integer))
                    &&
                    fmap squashMulId (parseRing "1 * (2 + 2)" :: Maybe (RingExpr Integer))
                    == fmap newSquashIds (parseRing "1 * (2 + 2)" :: Maybe (RingExpr Integer))

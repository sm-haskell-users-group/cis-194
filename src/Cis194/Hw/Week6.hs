{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Cis194.Hw.Week6 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object x') = Object $ fmap ynToBool x'
ynToBool (Array x') = Array $ fmap ynToBool x'
ynToBool x' = x'

parseData :: B.ByteString -> Either String Value
parseData = (fmap ynToBool) . eitherDecode

data Market = Market { fmid :: Int
                     , marketname :: T.Text
                     , website :: T.Text
                     , location :: T.Text
                     , street :: T.Text
                     , city :: T.Text
                     , county :: T.Text
                     , state :: T.Text
                     , zip :: T.Text
                     , x :: Double
                     , y :: Double

                     , bakedgoods :: Bool
                     , cheese :: Bool
                     , eggs :: Bool
                     , flowers :: Bool
                     , herbs :: Bool
                     , honey :: Bool
                     , jams :: Bool
                     , maple :: Bool
                     , meat :: Bool
                     , nuts :: Bool
                     , poultry :: Bool
                     , seafood :: Bool
                     , vegetables :: Bool
                     , wine :: Bool

                     , crafts :: Bool
                     , nursery :: Bool
                     , plants :: Bool
                     , soap :: Bool
                     , trees :: Bool

                     , prepared :: Bool

                     , credit :: Bool
                     , sfmnp :: Bool
                     , snap :: Bool
                     , wic :: Bool
                     , wiccash :: Bool

                     , season1date :: T.Text
                     , season1time :: T.Text
                     , season2date :: T.Text
                     , season2time :: T.Text
                     , season3date :: T.Text
                     , season3time :: T.Text
                     , season4date :: T.Text
                     , season4time :: T.Text

                     , updatetime :: T.Text
                     }
    deriving (Show, Generic)

instance FromJSON Market

-- Is there a better way to do this?
resultToEither :: Result a -> Either String a
resultToEither (Error s) = Left s
resultToEither (Success r) = Right r

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bs = (parseData bs) >>= (resultToEither . fromJSON)

loadData :: IO [Market]
loadData = (B.readFile "data/markets.json") >>= (return . getOrFail . parseMarkets)
    where getOrFail (Right s) = s
          getOrFail (Left e) = fail e

data OrdList a = OrdList { getOrdList :: [a] }
    deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend (OrdList xs) (OrdList ys) = OrdList $ foldr insert xs ys
    where insert x' a = filter (< x') a ++ [x'] ++ filter (>= x') a

type Searcher m = T.Text -> [Market] -> m
search :: Monoid m => (Market -> m) -> Searcher m
search toM name mkts = foldr (mappend . toM) mempty $ filter ((T.isInfixOf name) . marketname) mkts

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 M.listToMaybe (search (:[]))

lastFound :: Searcher (Maybe Market)
lastFound = compose2 (M.listToMaybe . reverse) (search (:[]))

allFound :: Searcher [Market]
allFound = search (:[])

numberFound :: Searcher Int
numberFound = compose2 length allFound

data NtoSMarket = NtoSMarket { getNSMarket :: Market }
    deriving (Show)

instance Eq NtoSMarket where
  (==) (NtoSMarket l) (NtoSMarket r) = (y l) == (y r)

instance Ord NtoSMarket where
  compare (NtoSMarket l) (NtoSMarket r) = compare (y l) (y r)

orderedNtoS :: Searcher [Market]
orderedNtoS = compose2 ((fmap getNSMarket) . getOrdList) (search (OrdList . (:[]) . NtoSMarket))

-- Extra stuff we can learn from the data:
--   What does the comparison of Farmer's Markets with many food options vs
--   fewer options look like?

--   Let's start by splitting up the four quadrants of the US.
--   (Hawaii gets lumped into SW, Alaska into NW. Oh well.)

centerXY :: (Double, Double)
centerXY = (-101.4146873, 39.7820819)

splitQuads :: [Market] -> ([Market], [Market], [Market], [Market])
splitQuads mkts = foldr acc ([],[],[],[]) mkts
  where (midx, midy) = centerXY
        acc m (nw, ne, sw, se) | x m <  midx && y m >= midy = (m : nw, ne, sw, se)
        acc m (nw, ne, sw, se) | x m >= midx && y m >= midy = (nw, m : ne, sw, se)
        acc m (nw, ne, sw, se) | x m <  midx && y m <  midy = (nw, ne, m : sw, se)
        acc m (nw, ne, sw, se) | x m >= midx && y m <  midy = (nw, ne, sw, m : se)
        acc _ r = r

--   Count the number of food options a given market has:

type FoodOptions = Integer

countFoodOptions :: Market -> FoodOptions
countFoodOptions m = foldr (\x' a -> if x' m then a + 1 else a) 0 accessors
  where accessors =
          [ bakedgoods
          , cheese
          , eggs
          , flowers
          , herbs
          , honey
          , jams
          , maple
          , meat
          , nuts
          , poultry
          , seafood
          , vegetables
          , wine
          ]

--   Transform [Market] per-quadrant into OptionsToMarkets. This will get us to
--   the point where we can start reasoning about the number of options per
--   regional market.

type NumberOfMarkets = Int
type OptionsToMarkets = [(FoodOptions, NumberOfMarkets)]

regionalOptions :: [Market] -> (OptionsToMarkets, OptionsToMarkets, OptionsToMarkets, OptionsToMarkets)
regionalOptions mkts = (work nw, work ne, work sw, work se)
  where (nw, ne, sw, se) = splitQuads mkts
        work :: [Market] -> OptionsToMarkets
        work xs = fmap (\ys -> (head ys, length ys)) $ L.group $ L.sort $ fmap countFoodOptions xs

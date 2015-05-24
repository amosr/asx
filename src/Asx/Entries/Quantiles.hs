module Asx.Entries.Quantiles where

import qualified Data.IntMap as Map
import qualified Data.Vector as V

import Control.DeepSeq

import Debug.Trace (trace)

data QuantileMap
 = QuantileMap
            !(Map.IntMap Int)
            !Int
 deriving (Show,Eq,Ord,Read)

instance NFData QuantileMap where
 rnf (QuantileMap a b)
  = deepseq a (deepseq b ())


data Lookup
 = Lookup (V.Vector Int)
 deriving (Show,Eq,Ord,Read)

instance NFData Lookup where
 rnf (Lookup a)
  = deepseq a ()

empty :: QuantileMap
empty = QuantileMap Map.empty 0

merge :: QuantileMap -> QuantileMap -> QuantileMap
merge (QuantileMap l sl) (QuantileMap r sr)
 = QuantileMap
 ( Map.unionWith (+) l r )
 ( sl + sr )

insert :: Int -> QuantileMap -> QuantileMap
insert i (QuantileMap m s)
 = QuantileMap (Map.insertWith (+) i 1 m) (s+1)

quantiles :: Int -> QuantileMap -> Lookup
quantiles i (QuantileMap m s)
 = Lookup
 $ V.fromList
 $ go 0 0 (Map.toList m)
 where
  size = (s `div` i) `max` 1

  go _ _ [] = []
  go ix next vs@((v,count):rest)
   | next < ix+count
   = v : go ix (next+size) vs
   | otherwise
   =     go (ix+count) next rest

lookupQ :: Lookup -> Int -> Int
lookupQ (Lookup qs) val
 = V.foldl go 0 qs
 where
  go ix v
   = if v >= val
     then ix
     else ix + 1


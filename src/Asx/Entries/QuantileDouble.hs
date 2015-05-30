module Asx.Entries.QuantileDouble where

import qualified Asx.Entries.Quantiles as Q

import Control.DeepSeq

data QtDouble
 = QtDouble !Q.QuantileMap !Q.QuantileMap 
 deriving (Show,Eq,Ord,Read)

instance NFData QtDouble where
 rnf (QtDouble a b)
  = deepseq a (deepseq b ())


data Lookup
 = Lookup Q.Lookup Q.Lookup
 deriving (Show,Eq,Ord,Read)

instance NFData Lookup where
 rnf (Lookup a b)
  = deepseq a (deepseq b ())

empty :: QtDouble
empty = QtDouble Q.empty Q.empty

merge :: QtDouble -> QtDouble -> QtDouble
merge (QtDouble lneg lpos) (QtDouble rneg rpos)
 = QtDouble (Q.merge lneg rneg)
            (Q.merge lpos rpos)

insert :: Double -> QtDouble -> QtDouble
insert v (QtDouble qneg qpos)
 -- ignore NaNs
 | v /= v
 = QtDouble qneg qpos

 | isPos (toInt v)
 = QtDouble qneg (Q.insert (toInt v) qpos)
 | otherwise
 = QtDouble (Q.insert (negate $ toInt v) qneg) qpos

quantiles :: Int -> QtDouble -> Lookup
quantiles i (QtDouble qneg qpos)
 = Lookup
 ( Q.quantiles i qneg )
 ( Q.quantiles i qpos )

lookupQ :: Lookup -> Double -> Int
lookupQ (Lookup ln lp) v
 | isPos (toInt v)
 = Q.lookupQ lp (toInt v)
 | otherwise
 = negate $ Q.lookupQ ln $ negate $ toInt v


toInt :: Double -> Int
toInt v = round (v * 1000)

isPos :: Int -> Bool
isPos v = v >= 0


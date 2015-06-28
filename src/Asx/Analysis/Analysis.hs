module Asx.Analysis.Analysis where

import qualified Data.Map as Map
import Text.Printf
import Data.List (sort, sortBy)
import Data.Function (on)

row :: String -> (String, Double)
row s
 | [x,y] <- words s
 = (y, read x)
 | otherwise
 = error ("bad row: " ++ s)

mapOfFile :: String -> Map.Map String [Double]
mapOfFile ss
 = Map.fromList
 $ map (\(k,v) -> (k, [v]))
 $ map row
 $ lines ss

mapMerge :: Map.Map String [Double] -> Map.Map String [Double] -> Map.Map String [Double]
mapMerge
 = Map.unionWith (++)


mapby   :: ([Double] -> Double)
        -> Map.Map String [Double]
        -> [(Double, String)]
mapby f m
 = map (\(k,v) -> (f v, k))
 $ Map.toList m

averages
 = mapby (\ls -> sum ls / fromIntegral (length ls))

mults
 = mapby (foldl (*) 1)

medians
 = mapby (\ls -> mid $ sort ls)
 where
  mid []
   = -1
  mid ls
   = ls !! (length ls `div` 2)

mins
 = mapby minimum

analyses
 = [ ("avg", averages)
   , ("mul", mults)
   , ("mean", medians)
   , ("min", mins) ]


showResult :: [(Double, String)] -> String
showResult res
 = unlines
 $ map sho
 $ reverse
 $ sortBy (compare `on` fst) res
 where
  sho (k,v)
   = printf "%.6f" k ++ " " ++ v

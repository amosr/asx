module Asx.Analysis.Analysis where

import qualified Data.Map as Map
import Text.Printf
import Data.List (sort)

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
        -> Map.Map Double String
mapby f m
 = Map.fromList
 $ map (\(k,v) -> (f v, k))
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


showResult :: Map.Map Double String -> String
showResult mm
 = unlines
 $ map sho
 $ reverse
 $ Map.toList mm
 where
  sho (k,v)
   = printf "%.6f" k ++ " " ++ v

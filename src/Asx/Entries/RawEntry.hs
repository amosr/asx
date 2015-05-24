-- TODO:
-- * add squares and roots of values, after quantile #s are added.
-- (since monotonic I don't think they will affect quantile)
-- * windowed is probably a big bottleneck since it's converting to lists
--   but could be slicing the vector
--
{-# LANGUAGE OverloadedStrings #-}
module Asx.Entries.RawEntry where

import Asx.Entries.Company
import Asx.Entries.Gradient
import Asx.Entries.Window
import qualified Asx.Entries.Quantiles as Q
import qualified Asx.Entries.QuantileDouble as QD

import Control.Applicative
import Data.Csv
import Data.Either.Combinators

import Data.List (intercalate)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Map as Map

import Control.DeepSeq

import Text.Printf

data RawEntry
 = RawEntry
 { date         :: String
 , open         :: Double
 , high         :: Double
 , low          :: Double
 , close        :: Double
 , volume       :: Double
 , adjclose     :: Double
 }
 deriving (Show,Eq,Ord)

instance FromNamedRecord RawEntry where
    parseNamedRecord r
     = RawEntry
     <$> r .: "Date"
     <*> r .: "Open"
     <*> r .: "High"
     <*> r .: "Low"
     <*> r .: "Close"
     <*> r .: "Volume"
     <*> r .: "Adj Close"

data EntryError
 = EntryErrorCsv String
 deriving (Show,Eq,Ord)

decodeEntries :: BL.ByteString -> Either EntryError (V.Vector RawEntry)
decodeEntries bl
 = mapBoth EntryErrorCsv (V.reverse . snd)
 $ decodeByName 
 $ bl


data ChangeDays
 = ChangeDays
 { num_05pc_hi :: Int
 , num_05pc_lo :: Int
 , num_10pc_hi :: Int
 , num_10pc_lo :: Int
 , num_20pc_hi :: Int
 , num_20pc_lo :: Int
 , num_30pc_hi :: Int
 , num_30pc_lo :: Int
 }
 deriving (Eq, Ord, Show)

listOfChangeDays :: String -> ChangeDays -> [(String,Int)]
listOfChangeDays prefix ch
 = [(prefix ++ "_num_05pc_hi", num_05pc_hi ch)
   ,(prefix ++ "_num_05pc_lo", num_05pc_lo ch)
   ,(prefix ++ "_num_10pc_hi", num_10pc_hi ch)
   ,(prefix ++ "_num_10pc_lo", num_10pc_lo ch)
   ,(prefix ++ "_num_20pc_hi", num_20pc_hi ch)
   ,(prefix ++ "_num_20pc_lo", num_20pc_lo ch)
   ,(prefix ++ "_num_30pc_hi", num_30pc_hi ch)
   ,(prefix ++ "_num_30pc_lo", num_30pc_lo ch)]

data Features
 = Features
   [(String, ChangeDays)]
   [(String, Double)]
 deriving (Eq, Ord, Show)

features :: V.Vector RawEntry -> RawEntry -> Features
features history re
 = Features
   (  changedays "whole" history
   ++ changedays "c0"   (chunk 0 4)
   ++ changedays "c1"   (chunk 1 4)
   ++ changedays "c2"   (chunk 2 4)
   ++ changedays "c3"   (chunk 3 4)
   ++ changedays "e0"   (chunk 0 8)
   ++ changedays "e1"   (chunk 1 8)
   ++ changedays "e2"   (chunk 2 8)
   ++ changedays "e3"   (chunk 3 8)
   ++ changedays "e4"   (chunk 4 8)
   ++ changedays "e5"   (chunk 5 8)
   ++ changedays "e6"   (chunk 6 8)
   ++ changedays "e7"   (chunk 7 8)
   )
   (  grads "whole"  history
   ++ grads "c0"    (chunk 0 4)
   ++ grads "c1"    (chunk 1 4)
   ++ grads "c2"    (chunk 2 4)
   ++ grads "c3"    (chunk 3 4)
   ++ grads "e0"    (chunk 0 8)
   ++ grads "e1"    (chunk 1 8)
   ++ grads "e2"    (chunk 2 8)
   ++ grads "e3"    (chunk 3 8)
   ++ grads "e4"    (chunk 4 8)
   ++ grads "e5"    (chunk 5 8)
   ++ grads "e6"    (chunk 6 8)
   ++ grads "e7"    (chunk 7 8)

   ++ grows "whole"  history
   ++ grows "c0"    (chunk 0 4)
   ++ grows "c1"    (chunk 1 4)
   ++ grows "c2"    (chunk 2 4)
   ++ grows "c3"    (chunk 3 4)
   ++ grows "e0"    (chunk 0 8)
   ++ grows "e1"    (chunk 1 8)
   ++ grows "e2"    (chunk 2 8)
   ++ grows "e3"    (chunk 3 8)
   ++ grows "e4"    (chunk 4 8)
   ++ grows "e5"    (chunk 5 8)
   ++ grows "e6"    (chunk 6 8)
   ++ grows "e7"    (chunk 7 8)
   )

 where
  changedays prefix part =
   [(prefix ++ "_open",         days part open)
   ,(prefix ++ "_high",         days part high)
   ,(prefix ++ "_low",          days part low)
   ,(prefix ++ "_close",        days part close)
   ,(prefix ++ "_adjclose",     days part adjclose)
   ,(prefix ++ "_diff_oc",      days part diff_oc)
   ,(prefix ++ "_diff_oac",     days part diff_oac)
   ,(prefix ++ "_volume",     days part volume)
   ,(prefix ++ "_volume_on_open",days part vol_on_open)
   ,(prefix ++ "_open_on_volume",days part open_on_vol)
   ]

  grads prefix part =
   [(prefix ++ "_open",         grad part open)
   ,(prefix ++ "_high",         grad part high)
   ,(prefix ++ "_low",          grad part low)
   ,(prefix ++ "_close",        grad part close)
   ,(prefix ++ "_adjclose",     grad part adjclose)
   ,(prefix ++ "_diff_oc",      grad part diff_oc)
   ,(prefix ++ "_diff_oac",     grad part diff_oac)
   ,(prefix ++ "_volume",       grad part volume)
   ,(prefix ++ "_volume_on_open",grad part vol_on_open)
   ,(prefix ++ "_open_on_volume",grad part open_on_vol)
   ]

  grows prefix part =
   [(prefix ++ "_avg_open",          avg_growth $ V.map open part)
   ,(prefix ++ "_avg_high",          avg_growth $ V.map high part)
   ,(prefix ++ "_avg_low",           avg_growth $ V.map low part)
   ,(prefix ++ "_avg_close",         avg_growth $ V.map close part)
   ,(prefix ++ "_avg_adjclose",      avg_growth $ V.map adjclose part)
   ,(prefix ++ "_avg_diff_oc",       avg_growth $ V.map diff_oc part)
   ,(prefix ++ "_avg_diff_oac",      avg_growth $ V.map diff_oac part)
   ,(prefix ++ "_volume",            avg_growth $ V.map volume part)
   ,(prefix ++ "_volume_on_open",    avg_growth $ V.map vol_on_open part)
   ,(prefix ++ "_open_on_volume",    avg_growth $ V.map open_on_vol part)
   ]

  diff_oc e  = close e - open e
  diff_oac e = adjclose e - open e
  vol_on_open e = volume e `divvy` open e
  open_on_vol e = open e   `divvy` volume e

  chunk num total
   = let len = V.length history
         sz  = len `div` total
     in  V.slice (num*sz) sz history


  days hist pricer
     = ChangeDays
     { num_05pc_hi = num_price_ratio hist pricer (between 1.0  1.05)
     , num_05pc_lo = num_price_ratio hist pricer (between 0.95 1.0)
     , num_10pc_hi = num_price_ratio hist pricer (between 1.05 1.1)
     , num_10pc_lo = num_price_ratio hist pricer (between 0.95 1.0)
     , num_20pc_hi = num_price_ratio hist pricer (between 1.2 1.3)
     , num_20pc_lo = num_price_ratio hist pricer (between 0.7 0.8)
     , num_30pc_hi = num_price_ratio hist pricer (>= 1.3)
     , num_30pc_lo = num_price_ratio hist pricer (< 0.7)
     }

  grad hist pricer
   = gradient
   $ zip [0..]
   $ map pricer
   $ V.toList hist

  between x y z = z >= x && z < y

  num_price_ratio hist pricer f
   = let p = pricer re
     in V.length
      $ V.filter (\h -> f (pricer h `divvy` p)) hist

divvy x y
 | y == 0
 = 0
 | otherwise
 = x / y


avg_growth :: V.Vector Double -> Double
avg_growth future
 = let chunksz = V.length future `div` 4
   in  V.sum (V.drop (chunksz * 3) future) `divvy` V.sum (V.take chunksz future)

predict_growth hist e future
 = let histlen    = 10
       histrecent = V.map open $ V.drop (V.length hist - histlen) hist
       histavg    = V.sum histrecent `divvy` fromIntegral histlen

       future'    = V.map open future
       futavg     = V.sum future' `divvy` fromIntegral (V.length future)
   in  futavg `divvy` histavg

predict e future
 = gradient
 $ zip [0..]
 $ map (\f -> low f `divvy` low e)
 $ V.toList future


daysHistory = 200
daysFuture  = 50
minVolume = 100000
daysAboveMin = 100

filterVolumes :: V.Vector RawEntry -> Bool
filterVolumes vols
 = (V.length $ V.filter (>minVolume) $ V.map volume vols) > daysAboveMin

filterMissing :: V.Vector RawEntry -> Bool
filterMissing vs
 = let dates = V.map (dateOf.date) vs
       steps = V.zip dates (V.drop 1 dates)
       diffs = V.map diffD steps
       maxi  = V.maximum diffs
   in  maxi  < 2
 where
  -- a few days missing isn't a big deal
  -- so it's easier to count months
  diffD ((y,m,d), (y',m',d'))
   = let years = y' - y
         mm    = m - (years * 12)
     in m' - mm

  dateOf :: String -> (Int,Int,Int)
  dateOf [y1,y2,y3,y4, '-', m1, m2, '-', d1, d2]
   = (read [y1,y2,y3,y4], read [m1,m2], read [d1,d2])

filterNotZero :: RawEntry -> Bool
filterNotZero e
 =  open e > 0
 && close e > 0
 && low e > 0
 && high e > 0
 && adjclose e > 0


trainEntries stride records
 = map (\(pre,e,post) -> (features pre e, date e, predict_growth pre e post))
 $ filter (\(pre,e,post) -> filterMissing (pre V.++ post))
 $ filter (\(pre,e,post) -> filterVolumes pre)
 $ strided stride
 $ V.toList
 $ windowed daysHistory daysFuture
 $ V.filter filterNotZero
 $ records
 where
  strided _ []
   = []
  strided n (x:xs)
   = x : strided n (drop n xs)

predictEntries records
 = map (\(pre,e,post) -> (features pre e, date e))
 $ filter (\(pre,e,post) -> filterMissing pre)
 $ filter (\(pre,e,post) -> filterVolumes pre)
 $ V.toList
 $ lastOfVec
 $ windowed daysHistory 0
 $ V.filter filterNotZero
 $ records
 where
  lastOfVec v
   = V.drop (V.length v - 1) v


addQuantilesToFeature :: FeaturesQL -> Features -> Features
addQuantilesToFeature (FeaturesQL qdays qgrads) (Features days grads)
 = Features days
  (grads ++ quantDays ++ quantGrads)
 where
  quantDays :: [(String, Double)]
  quantDays
   = concatMap lookupDay
   $ concatMap (uncurry listOfChangeDays)
   $ days

  quantGrads :: [(String, Double)]
  quantGrads
   = concatMap lookupGrad grads


  lookupDay :: (String, Int) -> [(String,Double)]
  lookupDay (nm,val)
   | Just ql <- Map.lookup nm qdays
   = mkEntry "day" nm $ Q.lookupQ ql val
   | otherwise
   = []

  lookupGrad :: (String, Double) -> [(String,Double)]
  lookupGrad (nm,val)
   | Just ql <- Map.lookup nm qgrads
   = mkEntry "grad" nm $ QD.lookupQ ql val
   | otherwise
   = []

  mkEntry :: String -> String -> Int -> [(String,Double)]
  mkEntry pre nm val
   = [ ("quant_" ++ pre ++ "_" ++ nm, fromIntegral val)
     , ("quant_" ++ pre ++ "_" ++ nm ++ "_" ++ show val, 1) ]


data FeaturesQ
 = FeaturesQ
   (Map.Map String Q.QuantileMap)
   (Map.Map String QD.QtDouble)
 deriving (Eq, Ord, Show)

instance NFData FeaturesQ where
 rnf (FeaturesQ a b)
  = deepseq a (deepseq b ())

emptyFQ :: FeaturesQ
emptyFQ  = FeaturesQ Map.empty Map.empty

mergeFQ :: FeaturesQ -> FeaturesQ -> FeaturesQ
mergeFQ (FeaturesQ ldays lgrads) (FeaturesQ rdays rgrads)
 = FeaturesQ
 ( Map.unionWith Q.merge  ldays rdays)
 ( Map.unionWith QD.merge lgrads rgrads)

mkQuantileMap :: [Features] -> FeaturesQ
mkQuantileMap fs
 = foldl join emptyFQ fs
 where
  join qs (Features days grads)
   = foldl addDays 
   ( foldl addGrad qs grads) days

  addGrad :: FeaturesQ -> (String,Double) -> FeaturesQ
  addGrad (FeaturesQ qdays qgrads) (nm,val)
   = FeaturesQ qdays
   $ Map.insertWith QD.merge nm (QD.insert val QD.empty) qgrads

  addDays :: FeaturesQ -> (String,ChangeDays) -> FeaturesQ
  addDays (FeaturesQ qdays qgrads) (nm,days)
   = FeaturesQ
   ( foldl addDay qdays
   $ listOfChangeDays nm days
   ) qgrads

  addDay :: Map.Map String Q.QuantileMap -> (String,Int) -> Map.Map String Q.QuantileMap
  addDay qdays (nm,val)
   = Map.insertWith Q.merge nm (Q.insert val Q.empty) qdays

data FeaturesQL
 = FeaturesQL
   (Map.Map String Q.Lookup)
   (Map.Map String QD.Lookup)
 deriving (Eq, Ord, Show, Read)

instance NFData FeaturesQL where
 rnf (FeaturesQL a b)
  = deepseq a (deepseq b ())


lookupFQ :: Int -> FeaturesQ -> FeaturesQL
lookupFQ i (FeaturesQ qdays qgrads)
 = FeaturesQL
 ( Map.map (Q.quantiles  i) qdays)
 ( Map.map (QD.quantiles i) qgrads)


showTrainedAsVw :: Company -> (Features, String, Double) -> String
showTrainedAsVw c (Features changes grads, date, label)
 = show label ++ " " ++ show importance ++ " '" ++ asxCode c ++ "-" ++ date ++ " | " ++ showChanges ++ " " ++ showGrads ++ "\n"
 where
  showChanges
   = intercalate " "
   $ concatMap showChange
   $ changes

  showChange (nm,ch)
   = map (\(t,v) -> (t ++ ":" ++ show v))
   $ listOfChangeDays nm ch

  showGrads
   = intercalate " "
   $ map (\(t,v) -> ("grad_" ++ t ++ ":" ++ show v))
   $ grads

  -- False negatives are better than false positives
  importance
   | label < 0
   = 5
   | otherwise
   = 1

showPredictAsVw :: Company -> (Features, String) -> String
showPredictAsVw c (f, date)
 = showTrainedAsVw c (f, date, 666)

showLabelAsVw :: Company -> (Features, String, Double) -> String
showLabelAsVw c (_, date, label)
 = printf "%.6f" label ++ " " ++ asxCode c ++ "-" ++ date ++ "\n"


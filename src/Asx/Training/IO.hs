module Asx.Training.IO where

import Asx.Entries.Company
import Asx.Entries.RawEntry

import Asx.CompanyList.IO

import Asx.Internal.File
import Asx.Internal.Http

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import qualified Control.Concurrent.ParallelIO as P
import qualified Control.Concurrent            as C

import qualified System.Directory as D
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Network.HTTP.Conduit   as H

import Data.List (intercalate)

import Text.Printf

data TrainingCommand
 = TrainPrint
 | TrainInto String
 | TrainLabels String

fileOfTrainingCommand :: TrainingCommand -> Maybe String
fileOfTrainingCommand TrainPrint = Nothing
fileOfTrainingCommand (TrainInto s) = Just s
fileOfTrainingCommand (TrainLabels s) = Just s

forCompanies
    :: (Int -> a -> Company -> IO a)
    -> (a -> a -> a)
    -> a
    -> [String]
    -> IO a
forCompanies exec join acc codes
 = do list <- getCompaniesList
      caps <- C.getNumCapabilities

      case list of
       Left err -> print err >> return acc
       Right cs
        -> if   null codes
           then do as <- P.parallel [V.foldM (exec i) acc (chunkI i caps cs) | i <- [0..caps-1]]
                   return $ foldl join acc as
           else   foldM (execFor cs) acc codes
 where
  execFor cs acc' code
   | Just c <- V.find ((==code) . asxCode) cs
   = exec 0 acc' c
   | otherwise
   = do putStrLn (code ++ ": invalid code")
        return acc'

  chunkI i caps cs
   = let sz = V.length cs `div` caps 
     in  if   i == caps-1
         then V.drop (sz*i) cs
         else V.slice (sz*i) sz cs


readCompany :: (V.Vector RawEntry -> IO a) -> a -> Company -> IO a
readCompany exec nothing c
 = do   putStr (asxCode c ++ ": ")
        fe <- D.doesFileExist (storagefile c)
        case fe of
         True  -> decode c
         False -> putStrLn "Missing; skipping" >> return nothing
 where
  decode c
   = do putStr "Reading: "
        res <- decodeEntries <$> BL.readFile (storagefile c)
        case res of
         Left err
          -> putStrLn ("Error: " ++ show err) >> return nothing
         Right contents
          -> exec contents


training future stride quants into codes
 = do -- Clear file
      maybe (return ()) (flip writeFile "") (fileOfTrainingCommand into)

      putStr "Reading quantiles: "
      qs <- readFile quants
      let q = read qs :: FeaturesQL
      seq q $ return ()
      putStrLn "OK"

      -- Run
      putStrLn "Computing data"
      forCompanies (\cap a c -> readCompany (run cap q c) () c) (\_ _ -> ()) () codes
      return ()
      
 where
  run cap q c contents
   = do let trained = trainEntries future stride contents
            qt      = map (\(f,d,l) -> (addQuantilesToFeature q f, d, l)) trained
        case into of
         TrainPrint
          -> mapM_ print qt
         TrainInto i
          -> mapM_ (appendFile (i ++ show cap) . showTrainedAsVw c) qt
         TrainLabels i
          -> mapM_ (appendFile (i ++ show cap) . showLabelAsVw c) qt

        putStrLn "OK"

predict quants into codes
 = do -- Clear file
      maybe (return ()) (flip writeFile "") into

      putStr "Reading quantiles: "
      qs <- readFile quants
      let q = read qs :: FeaturesQL
      seq q $ return ()
      putStrLn "OK"

      -- Run
      putStrLn "Computing data"
      forCompanies (\cap a c -> readCompany (run cap q c) () c) (\_ _ -> ()) () codes
      return ()
      
 where
  run cap q c contents
   = do let preds   = predictEntries contents
            qt      = map (\(f,d) -> (addQuantilesToFeature q f, d)) preds

        maybe   
                (mapM_ print qt)
                (\i -> mapM_ (appendFile (i++show cap) . showPredictAsVw c) qt)
                into

        putStrLn "OK"



printQuantiles stride codes
 = do   q <- getQuantiles stride codes
        print q

writeQuantiles stride into codes
 = do   q <- getQuantiles stride codes
        writeFile into (show q)
        putStrLn "Done"


getQuantiles stride codes
 = do   q <- forCompanies readAndFold mergeFQ emptyFQ codes
        deepseq q $ putStrLn "Folds done."
        putStrLn "Converting to lookups"
        let los = lookupFQ 10 q
        return los
 where
  readAndFold _cap acc c
   = do q' <- readCompany get1q emptyFQ c
        let acc' = mergeFQ acc q'
        deepseq acc' $ return acc'

  get1q contents
   = do let trained = trainEntries 10 stride contents
        let feats   = map (\(f,_,_) -> f) trained
        let qm = mkQuantileMap feats
        putStrLn "ok"
        return qm


exportAsIvory _stride
 = do list <- getCompaniesList
      case list of
       Left err -> print err
       Right cs -> mapM_ go $ V.toList cs
      return ()
 where
  go c
   = do fe <- D.doesFileExist (storagefile c)
        case fe of
         False -> return ()
         True
          -> do res <- decodeEntries <$> BL.readFile (storagefile c)
                case res of
                 Left err -> print err
                 Right r' -> mapM_ (putStr . printAsIvory c) $ interp $ V.toList r'

  interp [] = []
  interp [v] = [v]
  interp (v:u:vs)
   = [interp_hr v n u | n <- [0..23]]
     ++ interp (u:vs)

  interp_hr v n u
   = let lerp x y = x * (1 - (n / 24)) + y * (n / 24)
         lerpon f = lerp (f v) (f u)
     in v { date = date v ++ "T" ++ pad_hr (show (truncate n)) ++ ":00:00Z"
          , open = lerpon open
          , high = lerpon high
          , low  = lerpon low
          , close= lerpon close
          , volume= lerpon volume
          , adjclose = lerpon adjclose
     }

  pad_hr [x] = '0' : [x]
  pad_hr xs  = xs

  printAsIvory c r
   =  jso (asxCode c) (date r)
          [("open", flo $ open r)
          ,("high", flo $ high r)
          ,("low", flo $ low r)
          ,("close", flo $ close r)
          ,("volume", flo $ volume r)
          ,("adjclose", flo $ adjclose r)
          ,("category", str $ gicsGroup c)
          ,("name",     str $ companyName c)
          ]

  flo x = (show (truncate (x * 100000) :: Int))
  str x = show x

  jso cod dat xs
   = row cod dat "data" ("{" ++ (intercalate "," $ map (\(a,b) -> show a ++ ":" ++ b) xs) ++ "}")
   ++ concat [row cod dat nm val | (nm,val) <- xs]

  row cod dat nm val
   = cod ++ "|" ++ nm ++ "|" ++ val ++ "|" ++ dat ++ "\n"


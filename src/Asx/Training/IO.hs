module Asx.Training.IO where

import Asx.Entries.Company
import Asx.Entries.RawEntry

import Asx.CompanyList.IO

import Asx.Internal.File
import Asx.Internal.Http

import Control.Applicative
import Control.DeepSeq
import Control.Monad

import qualified System.Directory as D
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Network.HTTP.Conduit   as H

data TrainingCommand
 = TrainPrint
 | TrainInto String
 | TrainLabels String

fileOfTrainingCommand :: TrainingCommand -> Maybe String
fileOfTrainingCommand TrainPrint = Nothing
fileOfTrainingCommand (TrainInto s) = Just s
fileOfTrainingCommand (TrainLabels s) = Just s

forCompanies
    :: (a -> Company -> IO a)
    -> a
    -> [String]
    -> IO a
forCompanies exec acc codes
 = do list <- getCompaniesList

      case list of
       Left err -> print err >> return acc
       Right cs
        -> if   null codes
           then V.foldM exec         acc cs
           else   foldM (execFor cs) acc codes
 where
  execFor cs acc' code
   | Just c <- V.find ((==code) . asxCode) cs
   = exec acc' c
   | otherwise
   = do putStrLn (code ++ ": invalid code")
        return acc'


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


training stride quants into codes
 = do -- Clear file
      maybe (return ()) (flip writeFile "") (fileOfTrainingCommand into)

      putStr "Reading quantiles: "
      qs <- readFile quants
      let q = read qs :: FeaturesQL
      seq q $ return ()
      putStrLn "OK"

      -- Run
      putStrLn "Computing data"
      forCompanies (\a c -> readCompany (run q c) () c) () codes
      return ()
      
 where
  run q c contents
   = do let trained = trainEntries stride contents
            qt      = map (\(f,d,l) -> (addQuantilesToFeature q f, d, l)) trained
        case into of
         TrainPrint
          -> mapM_ print qt
         TrainInto i
          -> mapM_ (appendFile i . showTrainedAsVw c) qt
         TrainLabels i
          -> mapM_ (appendFile i . showLabelAsVw c) qt

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
      forCompanies (\a c -> readCompany (run q c) () c) () codes
      return ()
      
 where
  run q c contents
   = do let preds   = predictEntries contents
            qt      = map (\(f,d) -> (addQuantilesToFeature q f, d)) preds

        maybe   
                (mapM_ print qt)
                (\i -> mapM_ (appendFile i . showPredictAsVw c) qt)
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
 = do   q <- forCompanies readAndFold emptyFQ codes
        deepseq q $ putStrLn "Folds done."
        putStrLn "Converting to lookups"
        let los = lookupFQ 10 q
        return los
 where
  readAndFold acc c
   = do q' <- readCompany get1q emptyFQ c
        let acc' = mergeFQ acc q'
        deepseq acc' $ return acc'

  get1q contents
   = do let trained = trainEntries stride contents
        let feats   = map (\(f,_,_) -> f) trained
        let qm = mkQuantileMap feats
        putStrLn "ok"
        return qm



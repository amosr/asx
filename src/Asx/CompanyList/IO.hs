module Asx.CompanyList.IO where

import Asx.Entries.Company
import Asx.Internal.File
import Asx.Internal.Http

import Control.Applicative
import qualified Control.Concurrent.ParallelIO as P

import qualified System.Directory as D
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Network.HTTP.Conduit   as H

getCompaniesList
 = decodeCompanyList <$> BL.readFile "raw/ASXListedCompanies.csv"

grabAllCompanies force
 = do list <- getCompaniesList
      case list of
       Left err -> print err
       Right vs -> P.parallel_ (V.toList $ V.map checkExists vs)
 where
  checkExists c
   = do putStr (asxCode c ++ ": ")
        fe <- D.doesFileExist (storagefile c)
        case fe of
         True
          -> case force of
              True  -> putStr "File exists but continuing" >> get c
              False -> putStr "Skipping"
         False -> get c
        putStrLn ""


  get c
   = do putStr "Downloading: "
        res <- simpleHttpEither (downloadUrl c)
        case res of
         Left err
          -> putStr ("Error: " ++ showErr err)
         Right contents
          -> do writeBS (storagefile c) contents
                putStr "OK"


  showErr (H.StatusCodeException s _ _)
   = "status code: " ++ show s
  showErr other
   = show other



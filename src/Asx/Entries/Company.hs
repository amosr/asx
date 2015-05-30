{-# LANGUAGE OverloadedStrings #-}
module Asx.Entries.Company where

import Control.Applicative
import Data.Csv
import Data.Either.Combinators

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

data Company
 = Company
 { companyName  :: String
 , asxCode      :: String
 , gicsGroup    :: String
 }
 deriving (Show,Eq,Ord)

instance FromNamedRecord Company where
    parseNamedRecord r
     = Company
     <$> r .: "Company name"
     <*> r .: "ASX code"
     <*> r .: "GICS industry group"

data ListError
 = ListErrorCsv String
 deriving (Show,Eq,Ord)

decodeCompanyList :: BL.ByteString -> Either ListError (V.Vector Company)
decodeCompanyList bl
 = mapBoth ListErrorCsv snd
 $ decodeByName 
 $ dropLine
 $ dropLine
 $ bl

dropLine :: BL.ByteString -> BL.ByteString
dropLine
 = BL.drop 1 . BL.dropWhile (/= 10)


downloadUrl :: Company -> String
downloadUrl c
 = "http://real-chart.finance.yahoo.com/table.csv?s=" ++ asxCode c ++ ".AX"


storagefile :: Company -> String
storagefile c
 = "raw/" ++ cleanGroup c ++ "/" ++ clean (asxCode c) ++ ".csv"

cleanGroup :: Company -> String
cleanGroup c
 = clean (gicsGroup c)

clean = filter (not . flip elem bads)
 where
  bads  = " \t\r\n./\\&"



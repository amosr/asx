module Asx.Internal.File where

import qualified System.Directory as D
import qualified Data.ByteString.Lazy as BL

touchParentDir :: String -> IO ()
touchParentDir
 = D.createDirectoryIfMissing True
 . stripFile


stripFile :: String -> String
stripFile
 = reverse
 . drop 1
 . dropWhile (/='/')
 . reverse

writeBS :: String -> BL.ByteString -> IO ()
writeBS file contents
 =  touchParentDir file
 >> BL.writeFile   file contents


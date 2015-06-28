module Asx.Analysis.IO where

import Asx.Analysis.Analysis

import Control.Applicative

import qualified Data.Map as Map

analyse into files
 = do   ms <- mapM readMap files
        let all = foldl mapMerge Map.empty ms

        mapM_ (writeMap all) analyses
 where
  readMap file
   = mapOfFile <$> readFile file

  writeMap all (nm,anal)
   = writeFile (into ++ "." ++ nm) (showResult $ anal all)


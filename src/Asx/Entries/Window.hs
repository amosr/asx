{-# LANGUAGE BangPatterns #-}
module Asx.Entries.Window where
import qualified Data.Vector         as V

{-# INLINE windowed #-}
windowed    :: Int -> Int -> Int
            -> V.Vector e
            -> V.Vector (V.Vector e, e, V.Vector e)
windowed !back !forth !stride !elms
 = V.unfoldr go 0
 where
  !len = V.length elms
  !tot = back + forth + 1

  go i
   | i + tot <= len
   = Just ((V.unsafeSlice i back elms, V.unsafeIndex elms (i+back), V.unsafeSlice (i+back+1) forth elms), i + stride + 1)
   | otherwise
   = Nothing


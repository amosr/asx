module Asx.Entries.Window where
import qualified Data.Vector as V

windowed    :: Int -> Int
            -> V.Vector e
            -> V.Vector (V.Vector e, e, V.Vector e)
windowed back forth elms
 -- dumb simple
 = V.fromList
 $ go [] 
 $ V.toList elms
 where
  go _ []
   = []
  go seen (e:es)
   = let pre  = take back seen
         post = take forth es
     in  if    length pre  == back
            && length post == forth
         then  (V.fromList (reverse pre), e, V.fromList post) : go (e : seen) es
         else                           go (e : seen) es


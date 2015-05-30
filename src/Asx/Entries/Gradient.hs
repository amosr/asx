module Asx.Entries.Gradient where
import qualified Data.Vector as V

gradient :: V.Vector (Double,Double) -> Double
gradient gs
 = let sums        = V.map   (\(x,y) -> (x, y, x*x, x*y)) gs
       (x,y,xx,xy) = V.foldl (\(a,b,c,d) (e,f,g,h) -> (a+e, b+f, c+g, d+h)) (0,0,0,0) sums
       count       = fromIntegral $ V.length gs
       z           = (xx * count) - (x * x)
   in  ((xy * count) - (x * y)) `divvy` z


stddev   :: V.Vector Double -> Double
stddev   gs
 = let sums        = V.map   (\y -> (y, y*y)) gs
       (x,xx)      = V.foldl (\(a,b) (e,f) -> (a+e, b+f)) (0,0) sums
       count       = fromIntegral $ V.length gs
       xx'         = xx `divvy` count
       x'          = x  `divvy` count
   in  sqrt ( abs (xx' - x' * x') )


divvy x y
 | y == 0 || y /= y
 = 0
 | otherwise
 = x / y


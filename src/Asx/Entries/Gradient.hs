module Asx.Entries.Gradient where

gradient :: [(Double,Double)] -> Double
gradient gs
 = let norm        = case gs of
                      [] -> 1
                      ((_,0):_) -> 1
                      ((_,y):_) -> y
       norms       = map   (\(x,y) -> (x, y / norm))    gs

       sums        = map   (\(x,y) -> (x, y, x*x, x*y)) norms
       (x,y,xx,xy) = foldl (\(a,b,c,d) (e,f,g,h) -> (a+e, b+f, c+g, d+h)) (0,0,0,0) sums
       count       = fromIntegral $ length gs
       z           = (xx * count) - (x * x)
   in  if   z == 0
       then 0
       else ((xy * count) - (x * y)) / z



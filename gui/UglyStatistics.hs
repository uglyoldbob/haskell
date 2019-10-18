module UglyStatistics where

import Data.List as List

notEmpty :: [Double] -> [Double]
notEmpty [] = [1.0]
notEmpty l = l

discreteCDF :: [Double] -> Int -> [(Double, Double)]
discreteCDF x n = zip bins scaledbinvals
 where dx = notEmpty x
       dn = fromIntegral n
       sorted = List.sort dx
       minval = head sorted
       maxval = last sorted
       total = fromIntegral $ length dx
       bins = [minval + (maxval-minval)*(fromIntegral t) / dn | t <- [1..n]]
       binvals = map (lessOrEqualCount dx) bins
       dbinvals = map fromIntegral binvals
       scaledbinvals = map (/ total) dbinvals

lessOrEqualCount l n = length (filter (<= n) l)
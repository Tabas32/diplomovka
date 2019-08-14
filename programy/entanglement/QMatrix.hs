module QMatrix where

import Data.Complex

-- ================================================================
-- Matrix Operations
-- ADDITIONAL:
-- addM  ... matrix addition 
-- mulMS ... matrix-scalar multiplication
-- ================================================================
-- APPLIED IN QUANTUM COMPUTING:
-- mulSM ... scalar-matrix multiplication
-- mulM  ... matrix multiplication
-- mulT  ... tenzor product
-- ---------------------------------------------------------------- 

-- addM :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
addM     []        []    = []
addM (xrow:xM) (yrow:yM) = zipWith (+) xrow yrow : addM xM yM

-- mulMS :: [[Complex Double]] -> Complex Double -> [[Complex Double]]
mulMS xM s = map (map (*s)) xM

-- mulSM :: Complex Double -> [[Complex Double]] -> [[Complex Double]]
mulSM s yM = map (map (s*)) yM

-- mulM :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
mulM xM yM = [map (`scalarprod` x) (transpose yM) | x<-xM]

-- mulT :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
mulT xM yM = concat (map singlerow xM)
  where singlerow x = map concat (transpose (map (`mulSM` yM) x))

-- where local functions are

-- transpose :: [[Complex Double]] -> [[Complex Double]] -- just for mulM
-- THIS TYPE must be polymorph !!!!!!! it has different instances in mulM and mulT
transpose m | head m == [] = []
            | otherwise    = map head m : transpose (map tail m)

-- scalarprod ::[Complex Double] -> [Complex Double] -> Complex Double
scalarprod v1 v2 = sum (zipWith (*) v1 v2)

-- ---------------------------------------------------------------- 

-- ================================================================
-- End of module
-- ================================================================

-- TESTS:

m1 = [[1,2,3],
      [2,1,3]]

m2 = [[2,2],
      [3,1],
      [2,0]]

tm2 = [[2,3,2],
       [2,1,0]]


{-
> map (map (:+ 0.0)) (map (map fromInteger) m1)
[[1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0],[2.0 :+ 0.0,1.0 :+ 0.0,3.0 :+ 0.0]]
> map (map (:+ 0.0)) (map (map fromInteger) m2)
[[2.0 :+ 0.0,2.0 :+ 0.0],[3.0 :+ 0.0,1.0 :+ 0.0],[2.0 :+ 0.0,0.0 :+ 0.0]]
-}

mc1 = [[1.0 :+ 0.0,2.0 :+ 0.0, 3.0 :+ 0.0],
       [2.0 :+ 0.0,1.0 :+ 0.0, 3.0 :+ 0.0]]

mc2 = [[2.0 :+ 0.0,2.0 :+ 0.0],
       [3.0 :+ 0.0,1.0 :+ 0.0],
       [2.0 :+ 0.0,0.0 :+ 0.0]]



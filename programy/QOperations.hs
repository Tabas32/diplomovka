module QOperations where

import Data.Complex
import QMatrix
import QDefinitions

{- This module contains the following useful operations:

(|*)   ... binary operation that applies unitary operation u to state q 
(|=)   ... binary comparison of two states q1 q2 (or  unitary operations u1 u2)  

(|<)   ... true if state q is element of in set (or bag) of states 
(|-)   ... two sets (bags) of states difference 

convuq ... unary conversion operation of vector a(lpha),b(eta)
       ... to ((srea,nima),(sreb,nimb)) form
invconvuq ... inverse to convuq

(conv,invconv are local functions applicable to single complex number

rounduq ... normalization of complex numbers in U or Q by rounding to 3 decimals 

norm ... norm of vector of single bit state

-}

-- ================================================================
-- Application of Unitary Operation (|*) to State
 
-- Inputs: u ... unitary transformation, q ... state
-- Output: u |* q    ... next state
-- Note: (|*) = mulM ... (this operation is matrix multiplication)
-- ---------------------------------------------------------------- 

(|*) :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]] 
(|*) u q = mulM u q
-- ----------------------------------------------------------------

-- ================================================================
-- Comparison of two states
 
-- Inputs: vector states (alpha1,beta1) and (alpha2,beta2)
-- represented by column matrices
-- Output: True  ... if states are approx the same 
--         False ... otherwise
-- Note: Condition of approximated equality: eps < 0.01, i.e. 1% of 1 
-- ---------------------------------------------------------------- 
(|=) :: [[Complex Double]] -> [[Complex Double]] -> Bool
(|=) [[a1],[b1]] [[a2],[b2]] = abs (realPart a1 - realPart a2) < eps &&
                               abs (imagPart a1 - imagPart a2) < eps &&
                               abs (realPart b1 - realPart b2) < eps &&
                               abs (imagPart b1 - imagPart b2) < eps
  where eps = 0.01 -- 1% of 1.0
-- ----------------------------------------------------------------

-- ================================================================
-- State in Set of States operation

(|<) :: [[Complex Double]] -> [[[Complex Double]]] -> Bool
q |< qs = foldr (||) False (map (q |=) qs)  
-- ---------------------------------------------------------------- 

-- ================================================================
-- State Sets difference operation

(|-) :: [[[Complex Double]]] -> [[[Complex Double]]] -> [[[Complex Double]]]
qs1 |- qs2 = [ q | q <- qs1, not (q |< qs2) ]  
-- ---------------------------------------------------------------- 


-- ================================================================
-- norm of single bit state - column vector of complex numbers a(lpha),b(eta)

norm :: [[Complex Double]] -> Double

norm ([[a],[b]]) = sqrt ((magnitude a)^2 + (magnitude b)^2) 


-- ================================================================
-- Conversion of Transitions/States using conv
  
-- Input: Re, Im parts of u/q in form ((1/sqrt 2)^n), or (-(1/sqrt 2)^n)
-- Output: Re, Im parts of u/q in form 
--             [[((conv Re of a,conv Im of a),(conv Re of b,conv Im of b))]]
-- where a states for alpha and b states for beta
-- ---------------------------------------------------------------- 
convuq :: [[Complex Double]] -> [[((Int,Int),(Int,Int))]]
convuq uq = [convuqrow uqrow | uqrow <- uq]
  where
    convuqrow    uqrow    = [convuqnumber uqnumber | uqnumber <- uqrow]
    convuqnumber uqnumber = (conv (realPart uqnumber), conv (imagPart uqnumber))
-- ---------------------------------------------------------------- 

-- ================================================================
-- Inverse Conversion of Transitions/States using invconv
  
-- Input: [[((signRe,nRe),(signIm,nIm))]]
-- Output: [[Re :+ Im]] in form ((1/sqrt 2)^n), or (-(1/sqrt 2)^n)
-- ---------------------------------------------------------------- 
invconvuq :: [[((Int,Int),(Int,Int))]] -> [[Complex Double]]
invconvuq snuq = [invconvuqrow snuqrow | snuqrow <- snuq]
  where
    invconvuqrow snuqrow = [invconvuqnumber snuqnumber | snuqnumber <- snuqrow]
    invconvuqnumber ((signRe,nRe),(signIm,nIm)) = invconv (signRe,nRe) :+ invconv (signIm,nIm)
-- ---------------------------------------------------------------- 

-- where the next functions - conv and invconv are local to convuq and invconvuq 

-- ================================================================
-- conv: Conversion of +- (1/sqrt 2)^n to (sign,n)
 
-- Input: ((1/sqrt 2)^n), or (-(1/sqrt 2)^n)
-- Output: (-2,0) ... underflow, (2,0) ... overflow, -- error
--         (0,0) = 0.0,  (-1,n) = -(1/sqrt 2)^n, (1,n) = (1/sqrt 2)^n
-- ---------------------------------------------------------------- 
conv :: Double -> (Int,Int)
conv x | x <= -1.0 - eps/2 = (-2,0)
       | x >=  1.0 + eps/2 = (2,0)
       | abs x < eps       = (0,0) 
       | x     < 0.0       = (-1, npow (abs x))
       | x     > 0.0       = ( 1, npow (abs x))
  where
    npow x | x < 1.0-eps/2 = 1 + npow (x*sqrt 2)      
           | otherwise     = 0
    eps = 0.01 -- 1% of 1.0
-- ----------------------------------------------------------------

-- ================================================================
-- invconv: Inverse conversion of (sign,n) to +- (1/sqrt 2)^n
  
-- Input: (sign,n) 
-- Output: +- (1/sqrt 2)^n
-- ---------------------------------------------------------------- 
invconv :: (Int,Int) -> Double
invconv (sign,n) | sign == 0  = 0.0
                 | sign == -1 = -(1/sqrt 2)^n
                 | sign ==  1 =  (1/sqrt 2)^n
                 | sign == -2 = -2.0 -- error
                 | sign ==  2 =  2.0 -- error
-- ----------------------------------------------------------------

-- ================================================================
-- Transition/States normalization uq
 
-- Input: [[Complex Double]] of uq - structure of accurate numbers 
-- Output: [[Complex Double]] structure with items rounded to x.xxx
-- ---------------------------------------------------------------- 

rounduq :: [[Complex Double]] -> [[Complex Double]]
rounduq uq = [rounduqrow uqrow | uqrow <- uq]
  where 
    rounduqrow    uqrow    = [rounduqnumber uqnumber | uqnumber <- uqrow]
    rounduqnumber uqnumber = round3 (realPart uqnumber) :+ round3 (imagPart uqnumber)   
         
-- where 

round3 x = fromInteger(round (x*1000))/1000
-- ---------------------------------------------------------------- 

-- ================================================================
-- End of module
-- ================================================================



{-

TEST:

> uT
[[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.7071067811865475 :+ 0.7071067811865475]]

> map (map conjugate) uT
[[1.0 :+ (-0.0),0.0 :+ (-0.0)],[0.0 :+ (-0.0),0.7071067811865475 :+ (-0.7071067811865475)]]

> mulT uX uX
[[0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0],
 [0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0],
 [0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
 [1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0]]

> uH |* qP
[[0.9999999999999998 :+ 0.0],[0.0 :+ 0.0]]

> uH |* q0
[[0.7071067811865475 :+ 0.0],[0.7071067811865475 :+ 0.0]]

> uH |* qR
[[0.4999999999999999 :+ 0.4999999999999999],[0.4999999999999999 :+ (-0.4999999999999999)]]

> [[(1.0 :+ 0.8)],[(1.0 :+ 0.8)]] |= [[(1.0 :+ 0.799)],[(1.001 :+ 0.8)]]
True

> [[(1.0 :+ 0.8)],[(1.0 :+ 0.8)]] |= [[(1.0 :+ 0.79)],[(1.001 :+ 0.8)]]
False

> q0 |= q0
True

> q0 |= qP
False


-}



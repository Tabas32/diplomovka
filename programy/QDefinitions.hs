module QDefinitions where

import QMatrix
import Data.Complex

{- 

Module QDefinitions contains 
- The definition of basic states (2x1 matrices, i.e. column vectors of complex numbers), and 
- Quantum standard unitary operations (2x2 rotation matrices of complex numbers)
-}

{-
At:

http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Complex.html

the following constructor and functions for complex numbers are available:
===========================================================================

!a :+ !a infix 6
forms a complex number from its real and imaginary rectangular components.

realPart :: Complex a -> a
Extracts the real part of a complex number.

imagPart :: Complex a -> a
Extracts the imaginary part of a complex number.

mkPolar :: Floating a => a -> a -> Complex a
Form a complex number from polar components of magnitude and phase.

cis :: Floating a => a -> Complex a
cis t is a complex value with magnitude 1 and phase t (modulo 2*pi).

polar :: RealFloat a => Complex a -> (a, a)
The function polar takes a complex number and returns a (magnitude, phase) pair in canonical form: 
the magnitude is nonnegative, and the phase in the range (-pi, pi]; 
if the magnitude is zero, then so is the phase.

magnitude :: RealFloat a => Complex a -> a
The nonnegative magnitude of a complex number.

phase :: RealFloat a => Complex a -> a
The phase of a complex number, in the range (-pi, pi]. If the magnitude is zero, then so is the phase.

conjugate :: Num a => Complex a -> Complex a
The conjugate of a complex number.
-------------------------------------------------------------------

At
https://quantumexperience.ng.bluemix.net/qx/experience

IBM Quantum Experience is available.

-}

-- ---------------------------------------------------------------- 

-- ================================================================
-- Imaginary unit definition
-- ---------------------------------------------------------------- 
i :: Complex Double
i = 0.0 :+ 1.0
-- ---------------------------------------------------------------- 

-- ================================================================
-- s function definition
-- 
-- note s 1 = 1/sqrt 2, s (s 1) = 1/sqrt 2/sqrt 2, etc.
-- i.e. it is Cauchy sequence function
-- ---------------------------------------------------------------- 
s :: Complex Double -> Complex Double
s x = x/sqrt 2
-- ---------------------------------------------------------------- 
 
-- ================================================================
-- Basic Quantum States Definition
-- ---------------------------------------------------------------- 

q0,q1,qP,qM,qR,qL :: [[Complex Double]]

q0 = [[1.0 :+ 0.0],
      [0.0 :+ 0.0]]

q1 = [[0.0 :+ 0.0],
      [1.0 :+ 0.0]]

qP = [[s (1.0 :+ 0.0)],
      [s (1.0 :+ 0.0)]]

qM = [[s (1.0    :+ 0.0)],
      [s ((-1.0) :+ 0.0)]]

qR = [[s (1.0 :+ 0.0)],
      [s (0.0 :+ 1.0)]]

qL = [[s (1.0 :+   0.0) ],
      [s (0.0 :+ (-1.0))]]

-- ---------------------------------------------------------------- 

-- ================================================================
-- Standard Unitary Operations Definition
-- ---------------------------------------------------------------- 

uX,uY,uZ,uH,uS,uSd,uT,uTd :: [[Complex Double]]

uX  = [[0.0 :+ 0.0, 1.0 :+ 0.0],
       [1.0 :+ 0.0, 0.0 :+ 0.0]]

uY  = [[0.0 :+ 0.0, 0.0 :+ (-1.0)],
       [0.0 :+ 1.0, 0.0 :+   0.0 ]]

uZ  = [[1.0 :+ 0.0,   0.0  :+ 0.0],
       [0.0 :+ 0.0, (-1.0) :+ 0.0]]

uH  = [[s (1.0 :+ 0.0), s (  1.0  :+ 0.0)],
       [s (1.0 :+ 0.0), s ((-1.0) :+ 0.0)]]

uS  = [[1.0 :+ 0.0, 0.0 :+ 0.0],
       [0.0 :+ 0.0, 0.0 :+ 1.0]]

uSd = [[1.0 :+ 0.0, 0.0 :+   0.0 ],
       [0.0 :+ 0.0, 0.0 :+ (-1.0)]]

uT  = [[1.0 :+ 0.0,    0.0 :+ 0.0 ],
       [0.0 :+ 0.0, s (1.0 :+ 1.0)]]

uTd = [[1.0 :+ 0.0,    0.0 :+   0.0  ],
       [0.0 :+ 0.0, s (1.0 :+ (-1.0))]]

-- ---------------------------------------------------------------- 

-- ================================================================
-- End of module
-- ================================================================







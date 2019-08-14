module QEntanglement where 

import Data.Complex
import QMatrix
import QDefinitions
import QOperations

-- --------------------------------------------------------------- 
-- IMPORTED FROM QOperations
-- --------------------------------------------------------------- 

-- (|*) :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
-- aplikacia: u |* q ... dava novy stav q' 

-- (|=) :: [[Complex Double]] -> [[Complex Double]] -> Bool
-- q1 |= q2  je True ak stavy q1 a q2 su rovnake
-- Pozn: operacia zohladnuje nepresnost vypoctu stavov. Pre unitarne
-- operacie mozno pouzit standardne u1 == u2

-- (|<) :: [[Complex Double]] -> [[[Complex Double]]] -> Bool
-- q |< qset   je True, ak stav q je prvkom mnoziny stavov qset
-- q |< qbag   je True, ak stav q je prvkom suboru stavov qbag

-- Pozn: operacia zohladnuje nepresnost vypoctu stavov, co pre 
-- prislusnost unitarnej operacie k mnozine nie je potrebne, teda treba
-- pouzit standardne u `elem` uset

-- (|-) :: [[[Complex Double]]] -> [[[Complex Double]]] -> [[[Complex Double]]]
-- qs1 |- qs2   rozdiel mnozin stavov, vysledkom je mnozina stavov z qs1
--              ktore nie su v mnozine qs2

-- (|+) :: [[[Complex Double]]] -> [[[Complex Double]]] -> [[[Complex Double]]]
-- qs1 |+ qs2    zjednotenie mnozin (suborov) stavov

-- (|==) :: [[[Complex Double]]] -> [[[Complex Double]]] -> Bool
-- qs1 |== qs2   porovnanie mnozin (suborov) stavov

-- (|^) :: [[[Complex Double]]] -> [[[Complex Double]]] -> [[[Complex Double]]]
-- qs1 |^ qs2   prienik mnozin (suborov) stavov

-- convuq :: [[Complex Double]] -> [[((Int,Int),(Int,Int))]]
-- aplikacia: convuq c   ... transformuje komplexne cislo c = re :+ im
--                           do tvaru ((s_re,n_re),s_im,n_im))
-- kde s je znamienko +1,-1 a n je mocnina 1/sqrt 2

-- invconvuq :: [[((Int,Int),(Int,Int))]] -> [[Complex Double]]
-- inverzna konverzia k funkcii convuq

-- rounduq :: [[Complex Double]] -> [[Complex Double]]
-- aplikacia: rounduq q   alebo rounduq u 
-- zaokruhlenie zloziek vektorov stavu, resp. unitarnej operacie na
-- 3 desatinne miesta

-- norm :: [[Complex Double]] -> Double
-- aplikacia: norm q
-- vypocet normy vektora stavu jedneho bitu.
-- 
-- ---------------------------------------------------------------

-- --------------------------------------------------------------- 
-- IMPORTED FROM QMatrix
-- --------------------------------------------------------------- 

-- mulSM :: Num b => b -> [[b]] -> [[b]]
-- nasobenie skalara a matice (resp.vektora)

-- addM :: Num c => [[c]] -> [[c]] -> [[c]]
-- scitanie matic (resp.vektorov)

-- --------------------------------------------------------------- 


-- ===============================================================
-- Mnozina zakladnych stavov
-- --------------------------------------------------------------- 

qs0  = [q0, q1, qP, qM, qR, qL]

-- --------------------------------------------------------------- 

-- ===============================================================
-- Mnozina unitarnych operacii
-- --------------------------------------------------------------- 

us   = [uX, uY, uZ, uI, uH, uS, uSd, uT, uTd] -- POZOR je pridana uI (Identita)

-- --------------------------------------------------------------- 


{-

magnitude :: RealFloat a => Complex a -> a
The nonnegative magnitude of a complex number.

phase :: RealFloat a => Complex a -> a
The phase of a complex number, in the range (-pi, pi]. If the magnitude is zero, then so is the phase.

conjugate :: Num a => Complex a -> Complex a
The conjugate of a complex number.

-}


-- --------------------------------------------------------------

-- ZISTOVANIE zloziek vektora stavu a pravdepodobnosti merania 
-- 0 a 1 pre 1. bit

-- --------------------------------------------------------------


-- ===============================================================
-- Vyber zloziek alpha a beta stplcoveho vektora stavu q
-- --------------------------------------------------------------- 

alpha q = head (head q)

beta  q = head (head (tail q))

-- --------------------------------------------------------------- 

-- ===============================================================
-- Vyber zloziek alpha a beta stplcoveho vektora stavu q
-- --------------------------------------------------------------- 
alphanorm2 q = (magnitude (alpha q))^2

betanorm2 q = (magnitude (beta q))^2


{- Test


> alpha q0
1.0 :+ 0.0

> beta q0
0.0 :+ 0.0

> qs0 -- Q_0 = {q0,q1,qP,qM,qR,qL}
[[[1.0 :+ 0.0],[0.0 :+ 0.0]],
 [[0.0 :+ 0.0],[1.0 :+ 0.0]],
 [[0.7071067811865475 :+ 0.0],
 [0.7071067811865475 :+ 0.0]],
 [[0.7071067811865475 :+ 0.0],[(-0.7071067811865475) :+ 0.0]],
 [[0.7071067811865475 :+ 0.0],[0.0 :+ 0.7071067811865475]],
 [[0.7071067811865475 :+ 0.0],[0.0 :+ (-0.7071067811865475)]]]

-- Pozor: obsahuje aj uI, teda cela mnozina hradiel je U = {X,Y,Z,I,H,S,Sd,T,Td}
> us
[[[0.0 :+ 0.0,1.0 :+ 0.0],[1.0 :+ 0.0,0.0 :+ 0.0]],
 [[0.0 :+ 0.0,0.0 :+ (-1.0)],[0.0 :+ 1.0,0.0 :+ 0.0]],
 [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,(-1.0) :+ 0.0]],
 [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,1.0 :+ 0.0]],
 [[0.7071067811865475 :+ 0.0,0.7071067811865475 :+ 0.0],[0.7071067811865475 :+ 0.0,(-0.7071067811865475) :+ 0.0]],
 [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.0 :+ 1.0]],
 [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.0 :+ (-1.0)]],
 [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.7071067811865475 :+ 0.7071067811865475]],
 [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.7071067811865475 :+ (-0.7071067811865475)]]]


-- doplnene hradlo identity I
> uI
[[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,1.0 :+ 0.0]]
> 

-- Vyber prvych zloziek alpha stavov z mnoziny qs0
> map alpha qs0
[1.0 :+ 0.0,0.0 :+ 0.0,0.7071067811865475 :+ 0.0,0.7071067811865475 :+ 0.0,0.7071067811865475 :+ 0.0,0.7071067811865475 :+ 0.0]

-- Vyber druhych zloziek beta stavov z mnoziny qs0
> map beta qs0
[0.0 :+ 0.0,1.0 :+ 0.0,0.7071067811865475 :+ 0.0,(-0.7071067811865475) :+ 0.0,0.0 :+ 0.7071067811865475,0.0 :+ (-0.7071067811865475)]

-- vypocet stvorcov noriem zlozky alpha (pravdepodobnosti namerania bitu 0)
> map alphanorm2 qs0
[1.0,0.0,0.4999999999999999,0.4999999999999999,0.4999999999999999,0.4999999999999999]

-- vypocet stvorcov noriem zlozky beta (pravdepodobnosti namerania bitu 1)
> map betanorm2 qs0
[0.0,1.0,0.4999999999999999,0.4999999999999999,0.4999999999999999,0.4999999999999999]
>

Da sa napr. overit, ze qR sa da vyjadrit superpoziciou stavov |0> a |1>,
teda qR = (alpha_qR)|0> + (beta_qR)|1>, cize programovo
vypoctom

>(((alpha qR) `mulSM` q0) `addM` ((beta qR) `mulSM` q1)) == qR
True   

PS: mulMS ... nasobenie skalara a matice (resp. vektora)
    addM  ... sucet matic (resp. vektorov) 

-}



-- ===============================================================
-- DOPLNKOVA INFORMACIA: Definicia kvantoveho automatu pre 1 kvantovy bit
-- --------------------------------------------------------------- 

{-

Definicia: Kvantovy automat

Kvantovy automat je stvorica

QA = (Q_0, U, d, Q)

kde 

Q_0 je zaciatocna konecna mnozina kvantovych stavov
U   je konecna mnozina unitarnych operacii 
d   je prechodova funkcia (delta)
Q   je celkova mnozina stavov dosiahnuta v procese odvodenia

Prechodova funkcia je zobrazenim 

d: Q x U -> Q 

Rovnice prechodovej funkcie d pre vypocet nasledujceho stavu q' z predosleho stavu q unitarnou operaciou u su v tvare   

d (q, u) = q', kde q' = u q

Ak je prechodova funkcia uplna, vtedy kvantovy automat QA je automatom s konecnym poctom stavov (QFA).

Ak je prechodova funkcia neuplna (obsahuje iba prechody z prvych n krokov odvodenia), potom nevieme rozhodnut, ci kvantovy automat QA je automatom s konecnym poctom stavov alebo s nekonecnym poctom stavov.

-}



-- ================================================================
-- End of module quantumAutomaton
-- ================================================================

module QExperiments where 

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
-- aplikacia: q1 |= q2    je True ak stavy q1 a q2 su rovnake
-- Pozn: operacia zohladnuje nepresnost vypoctu stavov. Pre unitarne
-- operacie mozno pouzit standardne u1 == u2

-- (|<) :: [[Complex Double]] -> [[[Complex Double]]] -> Bool
-- aplikacia: q |< qset   je True, ak stav q je prvkom mnoziny stavov qset
--            q |< qbag   je True, ak stav q je prvkom suboru stavov qbag
-- Pozn: operacia zohladnuje nepresnost vypoctu stavov, co pre 
-- prislusnost unitarnej operacie k mnozine nie je potrebne, teda treba
-- pouzit standardne u `elem` uset

-- (|-) :: [[[Complex Double]]] -> [[[Complex Double]]] -> [[[Complex Double]]]

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

-- ===============================================================
-- INITIAL SETS OF STATES and UNITARY OPERATIONS DEFINITION

-- 1. alternativa (realisticka)
qset  = [q0, q1, qP, qM, qR, qL]
uset  = [uX, uY, uZ, uH, uS, uSd, uT, uTd]

-- 2. alternativa (realisticka, uzitocna pre formovanie grafu z jedineho stavu)
-- qset  = [q0]
-- uset  = [uX, uY, uZ, uH, uS, uSd, uT, uTd]

-- 3. alternativa (nerealisticka, iba pre hradla X,Y,Z)
-- qset  = [q0, q1, qP, qM, qR, qL]
-- uset  = [uX, uY, uZ]

-- --------------------------------------------------------------- 
-- END OF DEFINITION
-- ===============================================================

-- ---------------------------------------------------------------
-- DEFINICIA FUNKCII EXPERIMENTU
-- 
-- Experiment: Odvodenie novych stavov zo stavov z predosleho kroku
--             krok za krokom, pre jednoduchost na zaklade funkcii
--             ktore su konstantami
-- ---------------------------------------------------------------

-- ---------------------------------------------------------------
-- Krok 0: Zaciatocny subor a mnozina stavov
-- PS: Subor (bag) (nie vsak v 0-tom kroku) moze obsahovat
--     rovnaky stav viacnasobne, mnozina nie!

qbag0 = qset 
qset0 = qset
-- ---------------------------------------------------------------

-- ---------------------------------------------------------------
-- Krok 1: Odvodenie noveho suboru qbag a novej mnoziny qset z mnoziny qset0 
-- qbag1 ... subor 48 novych stavov (u |* q) v tabulke 8x6 (po riadkoch)
-- qset1 ... mnozina stavov zo suboru qbag1 (neexistuju v nej rovnake stavy)

qbag1 = [ u |* q | u<-uset, q<-qset0 ]

qset1 = mkset qbag1 []
  where 
    mkset []       qs = qs
    mkset (q:qbag) qs | q |< qs   = mkset qbag qs
                      | otherwise = mkset qbag (q:qs)
-- ---------------------------------------------------------------

-- Krok 2: subor qbag  a mnozina qset novych stavov (u |* q) 
--         pre vsetky q z mnoziny qset1 vypocitanej v predoslom kroku 1  

qbag2 = [ u |* q | u<-uset, q<-qset1 ]

qset2 = mkset qbag2 []
  where 
    mkset []       qs = qs
    mkset (q:qbag) qs | q |< qs   = mkset qbag qs
                      | otherwise = mkset qbag (q:qs)
-- ---------------------------------------------------------------

-- Krok 3:  subor qbag  a mnozina qset novych stavov (u |* q) 
--         pre vsetky q z mnoziny qset2 vypocitanej v predoslom kroku 2
  
qbag3 = [ u |* q | u<-uset, q<-qset2 ]

qset3 = mkset qbag3 []
  where 
    mkset []       qs = qs
    mkset (q:qbag) qs | q |< qs   = mkset qbag qs
                      | otherwise = mkset qbag (q:qs)
-- ---------------------------------------------------------------
-- KONIEC DEFINICII FUNKCII EXPERIMENTU
-- ---------------------------------------------------------------

{-
TESTY:

Narast velkosti suborov a mnozin:
---------------------------------

> [length qbag0,length qbag1,length qbag2,length qbag3]
[6,48,192,536]

> [length qset0,length qset1,length qset2,length qset3]
[6,24,67,121]

zda sa, ze je mozne dosiahnut konecnou mnozinou hradiel uset
nekonecny pocet stavov.

2.alternativa - iba stav q0 = \ket{0} v zaciatocnej mnozine

> [length qbag0,length qbag1,length qbag2,length qbag3]
[1,8,32,144]
> [length qset0,length qset1,length qset2,length qset3]
[1,4,18,50]

zda sa, ze je mozne dosiahnut konecnou mnozinou hradiel uset
nekonecny pocet stavov ako v predoslom pripade, iba trochu pomalsie

3.alternativa - (iba Pauliho hradla X,Y,Z)

> [length qbag0,length qbag1,length qbag2,length qbag3]
[6,18,45,72]
> [length qset0,length qset1,length qset2,length qset3]
[6,15,24,24]

zda sa, ze iba hradlami X,Y a Z nie je mozne dosiahnut lubovolny pocet stavov,
lebo pocet stavov v 3.kroku je rovnaky ako v 2.kroku

Vypis stavov, unitarnych operacii a mnozinove operacie:
-------------------------------------------------------

Vypis prveho stavu z mnoziny qset:

> q0
[[1.0 :+ 0.0],[0.0 :+ 0.0]]

Vypis posledneho stavu z mnoziny qset:

> qL
[[0.7071067811865475 :+ 0.0],[0.0 :+ (-0.7071067811865475)]]

-- --------------------------

Zaokruhlenie zloziek vektorov stavu qL typu Double na 3 desatinne miesta:

> rounduq qL
[[0.707 :+ 0.0],[0.0 :+ (-0.707)]]

POZOR: Nikdy netreba zokruhlovat pocas vypoctu, iba pred vypisom, napr. v LaTeXu.

-- --------------------------

Konverzia stavu qL do formy vhodnej pre vyjadrenie pomocou s n = (1/sqrt 2)^n
 
[((s_re_a,n_re_a),(s_im_a,n_im_a)),    ...    pre a(lpha)
 ((s_re_b,n_re_b),(s_im_b,n_im_b))]    ...    pre b(eta)

> convuq qL
[[((1,1),(0,0))],[((0,0),(-1,1))]]

teda (1,1)  = (+1)*(1/sqrt 2)^1 = 0.7071067811865475
     (0,0)  = 0.0
     (-1,1) = (-1)*(1/sqrt 2)^1 = - 0.7071067811865475                         

Inverzna konverzia:

> invconvuq (convuq qL)
[[0.7071067811865475 :+ 0.0],[0.0 :+ (-0.7071067811865475)]]

POZOR: Konverziu treba robit na presnych cislach, NIE zaokruhlenych
POZN: Je zrejme, ze kompozicia (invconvuq . convuq) je identita.

-- --------------------------

Vypis mnoziny stavov v 0.kroku (prekopirovany z haskell platform a manualne doplneny o nove riadky
aby to bolo viditelne):

> qset0
[[[1.0 :+ 0.0],[0.0 :+ 0.0]],
 [[0.0 :+ 0.0],[1.0 :+ 0.0]],
 [[0.7071067811865475 :+ 0.0],[0.7071067811865475 :+ 0.0]],
 [[0.7071067811865475 :+ 0.0],[(-0.7071067811865475) :+ 0.0]],
 [[0.7071067811865475 :+ 0.0],[0.0 :+ 0.7071067811865475]],
 [[0.7071067811865475 :+ 0.0],[0.0 :+ (-0.7071067811865475)]]]

Normovany vypis qset0 (moze byt uzitocny pri zobrazeni pomocou TIKZ alebo ine
graficke spracovanie) (opat zarovnane manualne)
 
> map rounduq qset0
[[[1.0 :+ 0.0],[0.0 :+ 0.0]],
 [[0.0 :+ 0.0],[1.0 :+ 0.0]],
 [[0.707 :+ 0.0],[0.707 :+ 0.0]],
 [[0.707 :+ 0.0],[(-0.707) :+ 0.0]],
 [[0.707 :+ 0.0],[0.0 :+ 0.707]],
 [[0.707 :+ 0.0],[0.0 :+ (-0.707)]]]

Konvertovany vypis qset0 (moze byt uzitocny pri odvodeni Cauchyho postupnosti a vyjadreni v LaTeXu)
(opat zarovnane manualne)

> map convuq qset0
[[[((1,0),(0,0))],[((0,0),(0,0))]],
 [[((0,0),(0,0))],[((1,0),(0,0))]],
 [[((1,1),(0,0))],[((1,1),(0,0))]],
 [[((1,1),(0,0))],[((-1,1),(0,0))]],
 [[((1,1),(0,0))],[((0,0),(1,1))]],
 [[((1,1),(0,0))],[((0,0),(-1,1))]]]

-- ----------------------------------------------------------------

Podobne mozno vypocitat 

mnozinu unitarnych operacii

> uset

urcit jej dlzku

> length uset

zaoukruhlit vsetky zlozky vektorov

> map rounduq uset

pouzit konverziu na celu mnozinu
 
> map convuq uset

ako aj spatnu konverziu

> map invconvuq (map convuq uset)

POZN: to iste mozno urobit pre lubovolny subor (bag) a lubovolnu mnozinu v procese
odvodenia novych stavov,

napr.

> map convuq bag3
> map invconvuq (map convuq qset3)

atd.

-- -----------------------------------------------

Ak by sme chceli zistit, ktory stav v qbag1 je rovny stavu q0, potom mozeme pouzit vyraz 

> map (q0 |=) qbag1
[False,True,False,False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,True,False,False,False,True,False,False,False,False,False,True,False,False,False,False,False,True,False,False,False,False,False,True,False,False,False,False,False]

co upavime manualne na maticu 8x6

F,T,F,F,F,F,
F,F,F,F,F,F,
T,F,F,F,F,F,
F,F,T,F,F,F,
T,F,F,F,F,F,
T,F,F,F,F,F,
T,F,F,F,F,F,
T,F,F,F,F,F

a vidime, q0 je v tabulke tam, kde je T


-- -----------------------------------------------

Ak by sme potrebovali overit, ci kazdy stav v nejakom subore alebo mnozine ma normu 1
(co v jednobitovom kvantovom systeme musi platit), potom mozeme pouzit operaciu
norm vektora na kazdy vektor v subore, resp. mnozine a este aj zaokruhlit.

> map (round3) (map norm qbag1)
[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]

 
-}

-- ================================================================
-- End of module
-- ================================================================

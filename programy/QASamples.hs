module Qsamples where

import Data.Complex
import QMatrix
import QDefinitions
import QOperations



-- ===============================================================
-- Mnozina zakladnych stavov
-- --------------------------------------------------------------- 

qs0  = [q0, q1, qP, qM, qR, qL]

-- --------------------------------------------------------------- 

-- ===============================================================
-- Mnozina unitarnych operacii
-- --------------------------------------------------------------- 

us   = [uX, uY, uZ, uH, uS, uSd, uT, uTd] 

-- --------------------------------------------------------------- 


-- ===============================================================
-- Definicia kvantoveho automatu pre 1 kvantovy bit
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


-- ===============================================================



-- ========================================================================

-- quantumAutomaton 3 [q0] [uX]

-- printQA "QASample1.txt" 3 [q0] [uX] => qsample1

-- ------------------------------------------------------------------------

qsample1 :: ([[[Complex Double]]],
                    [[[Complex Double]]],
                    [[[Int]]],
                    [[[Complex Double]]],
                    Int,
                    [Char])

qsample1 =

   ([[[1.0 :+ 0.0],[0.0 :+ 0.0]]],
    [[[0.0 :+ 0.0,1.0 :+ 0.0],[1.0 :+ 0.0,0.0 :+ 0.0]]],
    [[[1]],
    [[0]]],
    [[[1.0 :+ 0.0],[0.0 :+ 0.0]],
    [[0.0 :+ 0.0],[1.0 :+ 0.0]]],
    2,
    "QFA")

-- ------------------------------------------------------------------------


-- ========================================================================

-- quantumAutomaton 3 [q0] us

-- printQA "QASample2.txt" 3 [q0] us => qsample2

-- ------------------------------------------------------------------------

qsample2 :: ([[[Complex Double]]],
                    [[[Complex Double]]],
                    [[[Int]]],
                    [[[Complex Double]]],
                    Int,
                    [Char])
qsample2 =

   ([[[1.0 :+ 0.0],[0.0 :+ 0.0]]],
    [[[0.0 :+ 0.0,1.0 :+ 0.0],[1.0 :+ 0.0,0.0 :+ 0.0]],
    [[0.0 :+ 0.0,0.0 :+ (-1.0)],[0.0 :+ 1.0,0.0 :+ 0.0]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,(-1.0) :+ 0.0]],
    [[0.7071067811865475 :+ 0.0,0.7071067811865475 :+ 0.0],[0.7071067811865475 :+ 0.0,(-0.7071067811865475) :+ 0.0]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.0 :+ 1.0]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.0 :+ (-1.0)]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.7071067811865475 :+ 0.7071067811865475]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.7071067811865475 :+ (-0.7071067811865475)]]],
    [[[1,2,0,3,0,0,0,0]],
    [[0,4,5,6,2,7,8,9],[10,0,7,11,5,1,12,8],[3,13,6,0,14,15,16,17]],
    [[7,1,4,18,4,4,4,4],[19,10,1,20,7,2,21,12],[20,22,3,1,15,14,23,24],[4,19,2,13,1,5,9,21],[25,26,21,27,12,9,2,1],[26,28,12,29,8,21,1,7],[2,5,10,22,10,10,10,10],[13,30,22,2,31,32,33,34],[35,25,9,36,21,8,5,2],[11,3,18,7,37,38,39,40],[31,14,15,41,6,3,24,16],[38,42,14,43,3,6,17,23],[44,45,23,46,24,17,14,3],[47,48,24,49,16,23,3,15]]],
    [[[1.0 :+ 0.0],[0.0 :+ 0.0]],
    [[0.0 :+ 0.0],[1.0 :+ 0.0]],
    [[0.0 :+ 0.0],[0.0 :+ 1.0]],
    [[0.7071067811865475 :+ 0.0],[0.7071067811865475 :+ 0.0]],
    [[0.0 :+ (-1.0)],[0.0 :+ 0.0]],
    [[0.0 :+ 0.0],[(-1.0) :+ 0.0]],
    [[0.7071067811865475 :+ 0.0],[(-0.7071067811865475) :+ 0.0]],
    [[0.0 :+ 0.0],[0.0 :+ (-1.0)]],
    [[0.0 :+ 0.0],[0.7071067811865475 :+ 0.7071067811865475]],
    [[0.0 :+ 0.0],[0.7071067811865475 :+ (-0.7071067811865475)]],
    [[0.0 :+ 1.0],[0.0 :+ 0.0]],
    [[0.0 :+ 0.7071067811865475],[0.0 :+ (-0.7071067811865475)]],
    [[0.0 :+ 0.0],[(-0.7071067811865475) :+ 0.7071067811865475]],
    [[0.0 :+ (-0.7071067811865475)],[0.0 :+ 0.7071067811865475]],
    [[0.7071067811865475 :+ 0.0],[0.0 :+ 0.7071067811865475]],
    [[0.7071067811865475 :+ 0.0],[0.0 :+ (-0.7071067811865475)]],
    [[0.7071067811865475 :+ 0.0],[0.4999999999999999 :+ 0.4999999999999999]],
    [[0.7071067811865475 :+ 0.0],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[0.0 :+ (-0.7071067811865475)],[0.0 :+ (-0.7071067811865475)]],
    [[(-1.0) :+ 0.0],[0.0 :+ 0.0]],
    [[(-0.7071067811865475) :+ 0.0],[0.7071067811865475 :+ 0.0]],
    [[0.0 :+ 0.0],[(-0.7071067811865475) :+ (-0.7071067811865475)]],
    [[0.0 :+ 0.7071067811865475],[0.0 :+ 0.7071067811865475]],
    [[0.7071067811865475 :+ 0.0],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[0.7071067811865475 :+ 0.0],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.7071067811865475 :+ 0.7071067811865475],[0.0 :+ 0.0]],
    [[0.7071067811865475 :+ (-0.7071067811865475)],[0.0 :+ 0.0]],
    [[0.4999999999999999 :+ 0.4999999999999999],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[(-0.7071067811865475) :+ (-0.7071067811865475)],[0.0 :+ 0.0]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[(-0.7071067811865475) :+ 0.0],[(-0.7071067811865475) :+ 0.0]],
    [[0.0 :+ 0.7071067811865475],[0.7071067811865475 :+ 0.0]],
    [[0.0 :+ 0.7071067811865475],[(-0.7071067811865475) :+ 0.0]],
    [[0.0 :+ 0.7071067811865475],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[0.0 :+ 0.7071067811865475],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[(-0.7071067811865475) :+ 0.7071067811865475],[0.0 :+ 0.0]],
    [[(-0.4999999999999999) :+ 0.4999999999999999],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[0.0 :+ (-0.7071067811865475)],[(-0.7071067811865475) :+ 0.0]],
    [[0.0 :+ (-0.7071067811865475)],[0.7071067811865475 :+ 0.0]],
    [[0.0 :+ (-0.7071067811865475)],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.0 :+ (-0.7071067811865475)],[0.4999999999999999 :+ 0.4999999999999999]],
    [[0.4999999999999999 :+ 0.4999999999999999],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[(-0.7071067811865475) :+ 0.0],[0.0 :+ 0.7071067811865475]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[0.4999999999999999 :+ 0.4999999999999999]],
    [[0.4999999999999999 :+ 0.4999999999999999],[0.7071067811865475 :+ 0.0]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[0.0 :+ 0.7071067811865475]],
    [[0.8535533905932735 :+ 0.3535533905932737],[0.1464466094067262 :+ (-0.3535533905932737)]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[0.7071067811865475 :+ 0.0]],
    [[(-0.4999999999999999) :+ (-0.4999999999999999)],[0.0 :+ 0.7071067811865475]],
    [[0.8535533905932735 :+ (-0.3535533905932737)],[0.1464466094067262 :+ 0.3535533905932737]]],
    50,
    "QA")


-- ------------------------------------------------------------------------


-- ========================================================================

-- quantumAutomaton 3 qs0 us

-- printQA "QASample3.txt" 3 qs0 us => qsample3

-- ------------------------------------------------------------------------

qsample3 :: ([[[Complex Double]]],
                    [[[Complex Double]]],
                    [[[Int]]],
                    [[[Complex Double]]],
                    Int,
                    [Char])

qsample3 =
   ([[[1.0 :+ 0.0],[0.0 :+ 0.0]],
    [[0.0 :+ 0.0],[1.0 :+ 0.0]],
    [[0.7071067811865475 :+ 0.0],[0.7071067811865475 :+ 0.0]],
    [[0.7071067811865475 :+ 0.0],[(-0.7071067811865475) :+ 0.0]],
    [[0.7071067811865475 :+ 0.0],[0.0 :+ 0.7071067811865475]],
    [[0.7071067811865475 :+ 0.0],[0.0 :+ (-0.7071067811865475)]]],
    [[[0.0 :+ 0.0,1.0 :+ 0.0],[1.0 :+ 0.0,0.0 :+ 0.0]],
    [[0.0 :+ 0.0,0.0 :+ (-1.0)],[0.0 :+ 1.0,0.0 :+ 0.0]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,(-1.0) :+ 0.0]],
    [[0.7071067811865475 :+ 0.0,0.7071067811865475 :+ 0.0],[0.7071067811865475 :+ 0.0,(-0.7071067811865475) :+ 0.0]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.0 :+ 1.0]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.0 :+ (-1.0)]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.7071067811865475 :+ 0.7071067811865475]],
    [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.7071067811865475 :+ (-0.7071067811865475)]]],
    [[[1,6,0,2,0,0,0,0],[0,7,8,3,6,9,10,11],[2,12,3,0,4,5,13,14],[15,16,2,1,5,4,17,18],[19,4,5,20,3,2,18,13],[21,22,4,23,2,3,14,17]],
    [[24,0,9,25,8,1,26,10],[9,1,7,27,7,7,7,7],[28,24,1,15,9,6,29,26],[7,28,6,12,1,8,11,29],[30,31,29,32,26,11,6,1],[31,33,26,34,10,29,1,9],[25,2,27,9,35,21,36,37],[38,39,17,40,18,14,4,2],[41,42,18,43,13,17,2,5],[3,27,44,8,22,45,46,47],[16,3,25,24,48,19,49,50],[51,52,13,53,14,18,5,3],[54,55,14,56,17,13,3,4],[4,35,48,57,16,25,50,58],[23,59,57,4,60,32,38,61],[5,21,35,62,12,27,37,63],[48,5,45,64,44,15,65,46],[20,23,62,5,34,66,39,41]],
    [[6,8,24,16,24,24,24,24],[12,44,16,6,19,48,58,67],[68,30,11,69,29,10,8,6],[27,15,12,7,21,35,63,70],[8,9,28,44,28,28,28,28],[33,68,10,71,11,26,9,8],[10,26,30,60,30,30,30,30],[11,10,31,66,31,31,31,31],[71,72,60,10,20,57,61,73],[29,11,33,74,33,33,33,33],[69,60,66,11,62,23,75,39],[45,19,21,76,27,12,70,36],[77,38,63,78,70,37,35,12],[61,41,70,79,36,63,12,21],[13,36,73,80,55,61,60,20],[58,13,81,82,75,41,34,23],[53,83,80,13,84,85,86,87],[14,37,75,88,39,81,23,66],[67,14,89,90,91,51,59,71],[56,92,88,14,93,94,95,96],[44,25,15,28,45,22,97,65],[35,45,22,59,15,44,47,97],[73,81,97,98,65,47,22,15],[75,89,65,99,46,97,15,45],[22,48,19,100,25,16,67,49],[52,73,58,101,67,50,48,16],[55,75,67,102,49,58,16,19],[17,63,91,103,42,89,71,76],[49,17,77,104,105,54,72,100],[40,106,103,17,107,108,109,110],[18,70,105,111,52,77,100,69],[50,18,61,112,73,38,57,60],[43,113,111,18,114,115,116,117],[100,57,20,19,32,60,73,55],[39,91,49,118,50,67,19,25],[64,20,76,45,74,71,91,42],[60,34,32,30,57,20,55,38],[37,65,55,92,38,73,20,32],[76,100,23,21,66,34,81,75],[81,51,36,119,37,70,21,27],[59,64,100,22,69,72,77,105],[105,61,47,120,97,46,44,22],[66,71,34,31,23,62,41,81]]],
    [[[1.0 :+ 0.0],[0.0 :+ 0.0]],
    [[0.0 :+ 0.0],[1.0 :+ 0.0]],
    [[0.7071067811865475 :+ 0.0],[0.7071067811865475 :+ 0.0]],
    [[0.7071067811865475 :+ 0.0],[(-0.7071067811865475) :+ 0.0]],
    [[0.7071067811865475 :+ 0.0],[0.0 :+ 0.7071067811865475]],
    [[0.7071067811865475 :+ 0.0],[0.0 :+ (-0.7071067811865475)]],
    [[0.0 :+ 0.0],[0.0 :+ 1.0]],
    [[0.0 :+ (-1.0)],[0.0 :+ 0.0]],
    [[0.0 :+ 0.0],[(-1.0) :+ 0.0]],
    [[0.0 :+ 0.0],[0.0 :+ (-1.0)]],
    [[0.0 :+ 0.0],[0.7071067811865475 :+ 0.7071067811865475]],
    [[0.0 :+ 0.0],[0.7071067811865475 :+ (-0.7071067811865475)]],
    [[0.0 :+ (-0.7071067811865475)],[0.0 :+ 0.7071067811865475]],
    [[0.7071067811865475 :+ 0.0],[0.4999999999999999 :+ 0.4999999999999999]],
    [[0.7071067811865475 :+ 0.0],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[(-0.7071067811865475) :+ 0.0],[0.7071067811865475 :+ 0.0]],
    [[0.0 :+ 0.7071067811865475],[0.0 :+ 0.7071067811865475]],
    [[0.7071067811865475 :+ 0.0],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[0.7071067811865475 :+ 0.0],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.0 :+ 0.7071067811865475],[0.7071067811865475 :+ 0.0]],
    [[0.4999999999999999 :+ 0.4999999999999999],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[0.0 :+ (-0.7071067811865475)],[0.7071067811865475 :+ 0.0]],
    [[(-0.7071067811865475) :+ 0.0],[0.0 :+ 0.7071067811865475]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[0.4999999999999999 :+ 0.4999999999999999]],
    [[0.0 :+ 1.0],[0.0 :+ 0.0]],
    [[0.0 :+ 0.7071067811865475],[0.0 :+ (-0.7071067811865475)]],
    [[0.0 :+ 0.0],[(-0.7071067811865475) :+ 0.7071067811865475]],
    [[0.0 :+ (-0.7071067811865475)],[0.0 :+ (-0.7071067811865475)]],
    [[(-1.0) :+ 0.0],[0.0 :+ 0.0]],
    [[0.0 :+ 0.0],[(-0.7071067811865475) :+ (-0.7071067811865475)]],
    [[0.7071067811865475 :+ 0.7071067811865475],[0.0 :+ 0.0]],
    [[0.7071067811865475 :+ (-0.7071067811865475)],[0.0 :+ 0.0]],
    [[0.4999999999999999 :+ 0.4999999999999999],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[(-0.7071067811865475) :+ (-0.7071067811865475)],[0.0 :+ 0.0]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.0 :+ (-0.7071067811865475)],[(-0.7071067811865475) :+ 0.0]],
    [[0.0 :+ (-0.7071067811865475)],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.0 :+ (-0.7071067811865475)],[0.4999999999999999 :+ 0.4999999999999999]],
    [[0.4999999999999999 :+ 0.4999999999999999],[0.7071067811865475 :+ 0.0]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[0.0 :+ 0.7071067811865475]],
    [[0.8535533905932735 :+ 0.3535533905932737],[0.1464466094067262 :+ (-0.3535533905932737)]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[0.7071067811865475 :+ 0.0]],
    [[(-0.4999999999999999) :+ (-0.4999999999999999)],[0.0 :+ 0.7071067811865475]],
    [[0.8535533905932735 :+ (-0.3535533905932737)],[0.1464466094067262 :+ 0.3535533905932737]],
    [[(-0.7071067811865475) :+ 0.0],[(-0.7071067811865475) :+ 0.0]],
    [[(-0.7071067811865475) :+ 0.0],[0.0 :+ (-0.7071067811865475)]],
    [[(-0.7071067811865475) :+ 0.0],[0.4999999999999999 :+ 0.4999999999999999]],
    [[(-0.7071067811865475) :+ 0.0],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[0.0 :+ 0.7071067811865475],[(-0.7071067811865475) :+ 0.0]],
    [[0.0 :+ 0.7071067811865475],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.0 :+ 0.7071067811865475],[0.4999999999999999 :+ 0.4999999999999999]],
    [[(-0.4999999999999999) :+ (-0.4999999999999999)],[0.7071067811865475 :+ 0.0]],
    [[(-0.4999999999999999) :+ 0.4999999999999999],[0.0 :+ 0.7071067811865475]],
    [[0.1464466094067262 :+ (-0.3535533905932737)],[0.8535533905932735 :+ 0.3535533905932737]],
    [[(-0.4999999999999999) :+ 0.4999999999999999],[0.7071067811865475 :+ 0.0]],
    [[0.4999999999999999 :+ 0.4999999999999999],[0.0 :+ 0.7071067811865475]],
    [[0.1464466094067262 :+ 0.3535533905932737],[0.8535533905932735 :+ (-0.3535533905932737)]],
    [[0.4999999999999999 :+ 0.4999999999999999],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.0 :+ 0.7071067811865475],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[(-0.4999999999999999) :+ (-0.4999999999999999)],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.4999999999999999 :+ 0.4999999999999999],[0.4999999999999999 :+ 0.4999999999999999]],
    [[0.4999999999999999 :+ 0.4999999999999999],[0.0 :+ (-0.7071067811865474)]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[0.0 :+ (-0.7071067811865475)],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[(-0.4999999999999999) :+ 0.4999999999999999],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[(-0.7071067811865475) :+ 0.0],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[0.0 :+ 0.7071067811865475],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[(-0.7071067811865475) :+ 0.7071067811865475],[0.0 :+ 0.0]],
    [[(-0.4999999999999999) :+ 0.4999999999999999],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[0.0 :+ (-0.7071067811865475)],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[(-0.4999999999999999) :+ (-0.4999999999999999)],[0.4999999999999999 :+ 0.4999999999999999]],
    [[(-0.4999999999999999) :+ 0.4999999999999999],[(-0.4999999999999999) :+ 0.4999999999999999]],
    [[0.4999999999999999 :+ 0.4999999999999999],[(-0.7071067811865474) :+ 0.0]],
    [[(-0.4999999999999999) :+ (-0.4999999999999999)],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[(-0.7071067811865474) :+ 0.0]],
    [[(-0.4999999999999999) :+ (-0.4999999999999999)],[0.4999999999999999 :+ (-0.4999999999999999)]],
    [[(-0.4999999999999999) :+ 0.4999999999999999],[0.0 :+ (-0.7071067811865475)]],
    [[(-0.3535533905932737) :+ (-0.1464466094067262)],[0.3535533905932737 :+ (-0.8535533905932735)]],
    [[0.3535533905932737 :+ (-0.1464466094067262)],[(-0.3535533905932737) :+ (-0.8535533905932735)]],
    [[0.8535533905932735 :+ 0.3535533905932737],[(-0.1464466094067262) :+ 0.3535533905932737]],
    [[0.4999999999999999 :+ (-0.4999999999999999)],[0.0 :+ (-0.7071067811865475)]],
    [[0.3535533905932737 :+ 0.1464466094067262],[0.3535533905932737 :+ (-0.8535533905932735)]],
    [[(-0.3535533905932737) :+ (-0.1464466094067262)],[(-0.3535533905932737) :+ 0.8535533905932735]],
    [[0.8535533905932735 :+ 0.3535533905932737],[0.3535533905932737 :+ 0.1464466094067262]],
    [[0.8535533905932735 :+ 0.3535533905932737],[(-0.3535533905932737) :+ (-0.1464466094067262)]],
    [[0.8535533905932735 :+ 0.3535533905932737],[0.3535533905932736 :+ (-0.14644660940672619)]],
    [[0.8535533905932735 :+ 0.3535533905932737],[(-0.14644660940672619) :+ (-0.3535533905932736)]],
    [[0.8535533905932735 :+ (-0.3535533905932737)],[(-0.1464466094067262) :+ (-0.3535533905932737)]],
    [[(-0.4999999999999999) :+ (-0.4999999999999999)],[0.0 :+ (-0.7071067811865475)]],
    [[(-0.3535533905932737) :+ 0.1464466094067262],[(-0.3535533905932737) :+ (-0.8535533905932735)]],
    [[(-0.4999999999999999) :+ (-0.4999999999999999)],[(-0.7071067811865475) :+ 0.0]],
    [[0.3535533905932737 :+ (-0.1464466094067262)],[0.3535533905932737 :+ 0.8535533905932735]],
    [[0.8535533905932735 :+ (-0.3535533905932737)],[(-0.3535533905932737) :+ 0.1464466094067262]],
    [[0.8535533905932735 :+ (-0.3535533905932737)],[0.3535533905932737 :+ (-0.1464466094067262)]],
    [[0.8535533905932735 :+ (-0.3535533905932737)],[(-0.14644660940672619) :+ 0.3535533905932736]],
    [[0.8535533905932735 :+ (-0.3535533905932737)],[0.3535533905932736 :+ 0.14644660940672619]],
    [[(-0.7071067811865475) :+ 0.0],[(-0.4999999999999999) :+ (-0.4999999999999999)]],
    [[(-0.1464466094067262) :+ 0.3535533905932737],[(-0.8535533905932735) :+ (-0.3535533905932737)]],
    [[(-0.1464466094067262) :+ (-0.3535533905932737)],[(-0.8535533905932735) :+ 0.3535533905932737]],
    [[(-0.4999999999999999) :+ 0.4999999999999999],[0.4999999999999999 :+ 0.4999999999999999]],
    [[(-0.3535533905932737) :+ 0.8535533905932735],[0.3535533905932737 :+ 0.1464466094067262]],
    [[0.3535533905932737 :+ 0.8535533905932735],[(-0.3535533905932737) :+ 0.1464466094067262]],
    [[0.1464466094067262 :+ (-0.3535533905932737)],[(-0.8535533905932735) :+ (-0.3535533905932737)]],
    [[(-0.3535533905932737) :+ 0.8535533905932735],[(-0.3535533905932737) :+ (-0.1464466094067262)]],
    [[(-0.4999999999999999) :+ 0.4999999999999999],[(-0.7071067811865475) :+ 0.0]],
    [[0.3535533905932737 :+ (-0.8535533905932735)],[0.3535533905932737 :+ 0.1464466094067262]],
    [[0.1464466094067262 :+ (-0.3535533905932737)],[(-0.3535533905932737) :+ 0.8535533905932735]],
    [[0.1464466094067262 :+ (-0.3535533905932737)],[0.3535533905932737 :+ (-0.8535533905932735)]],
    [[0.1464466094067262 :+ (-0.3535533905932737)],[0.3535533905932736 :+ 0.8535533905932734]],
    [[0.1464466094067262 :+ (-0.3535533905932737)],[0.8535533905932734 :+ (-0.3535533905932736)]],
    [[0.1464466094067262 :+ 0.3535533905932737],[(-0.8535533905932735) :+ 0.3535533905932737]],
    [[0.3535533905932737 :+ 0.8535533905932735],[0.3535533905932737 :+ (-0.1464466094067262)]],
    [[(-0.3535533905932737) :+ (-0.8535533905932735)],[(-0.3535533905932737) :+ 0.1464466094067262]],
    [[0.1464466094067262 :+ 0.3535533905932737],[0.3535533905932737 :+ 0.8535533905932735]],
    [[0.1464466094067262 :+ 0.3535533905932737],[(-0.3535533905932737) :+ (-0.8535533905932735)]],
    [[0.1464466094067262 :+ 0.3535533905932737],[0.8535533905932734 :+ 0.3535533905932736]],
    [[0.1464466094067262 :+ 0.3535533905932737],[0.3535533905932736 :+ (-0.8535533905932734)]],
    [[0.3535533905932737 :+ 0.1464466094067262],[(-0.3535533905932737) :+ 0.8535533905932735]],
    [[0.3535533905932737 :+ (-0.8535533905932735)],[(-0.3535533905932737) :+ (-0.1464466094067262)]],
    [[(-0.8535533905932735) :+ 0.3535533905932737],[(-0.1464466094067262) :+ (-0.3535533905932737)]]],
    121,
    "QA")

-- ========================================================================

-- End of Samples

-- ========================================================================

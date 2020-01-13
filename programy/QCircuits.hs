module QCircuits where 

import Data.Complex
import QDefinitions
import QOperations
import QEntanglement

-- ----------------Structure for storing qvantum circuits

data Element = X
    | Y
    | Z
    | H
    | S
    | Sd
    | T
    | Td
    | Ct   -- CNOT target bit
    | Cc  -- CNOT control bit
    | E   -- empty
    deriving (Show, Eq)

type LevelGates = [Element]

-- Level :
--     LevelGates : [H1, H2, ... ]
--     measurePoint : T
data Level = Level LevelGates Bool deriving Show

type Circuit = [Level]

-- ----------------Structure for storing resutls

data R = R Double [Int]
    deriving (Show)
type T = [R]
type Ts = [T]


-- ----------------Structure for storing qbit states

type QBit = [[Complex Double]]
type States = [QBit]
type SubTrees = [StateTree]

-- StateTree :
--     probability : P of tree hapening
--     States : states of QBits in one moment
--     SubTrees : possible states of QBits down the circuit
data StateTree = StateTree Double States SubTrees deriving Show

-- Result Table structure
-- Ts -> list of results from end to start
data RT = RT StateTree Ts
    deriving (Show)

processCircuit :: Circuit -> RT -> RT 
processCircuit (l:ls) (RT st t) =
    let newStT = processLevel l st
    in if isLMeasured l
        then processCircuit ls (RT newStT (upTs t newStT))
        else processCircuit ls (RT newStT t)
processCircuit [] rt = rt

upTs :: Ts -> StateTree -> Ts
upTs t st = (collapseR st) : t

collapseR :: StateTree -> T
collapseR (StateTree p s []) = map (mulR p) (filter isRgt0 (collapseStates s))
collapseR (StateTree p s subTs) = foldr (++) [] (map collapseR subTs)

-- Returns True if level should be measured
isLMeasured :: Level -> Bool
isLMeasured (Level _ m) = m

{-
processCircuit :: Circuit -> StateTree -> StateTree 
processCircuit (l:ls) t = -- TODO : tu bude if t = true tak sprav novy Ts (cize vysledky)
    let newStateTree = processLevel l t
    in processCircuit ls newStateTree
processCircuit [] t = t
-}

-- Applies Level on leafs of StateTree
processLevel :: Level -> StateTree -> StateTree
processLevel l@(Level g t) st@(StateTree p s []) = StateTree p s (calculateStateTrees g s p)
processLevel l@(Level g t) st@(StateTree p s subts) = StateTree p s (map (processLevel l) subts)

-- Creates new StateTree list, using level operations
calculateStateTrees ::  LevelGates -> States -> Double -> [StateTree]
calculateStateTrees g s p = 
    if (Cc `elem` g)
    then applyCNot g s p
    else (StateTree p (zipWith applyGate g s) []) : []

-- Function splits StateTree depending on CNOT gate 
applyCNot ::  LevelGates -> States -> Double -> [StateTree]
applyCNot g s p = 
    let pasP = cnPasP g s p
    in if pasP == 0
        then [StateTree p s []]
        else (StateTree pasP (zipWith applyGate g s) []) : (StateTree (1 - pasP) s []) : []

-- Returns probability of control bits being 1
cnPasP :: [Element] -> [QBit] -> Double -> Double
cnPasP g s p = p * (foldr (*) 1 (filter (>=0) (zipWith probCt1 g s)))

-- Returns probablitiy of Cc being 1
-- if e is not Ct returns -1
probCt1 :: Element -> QBit -> Double
probCt1 e q
    | e == Cc = betanorm2 q
    | otherwise = -1

-- Applies gate to qbit state
applyGate :: Element -> QBit -> QBit
applyGate e q = 
    case e of
        X -> uX |* q
        Y -> uY |* q
        Z -> uZ |* q
        H -> uH |* q
        S -> uS |* q
        Sd -> uSd |* q
        T -> uT |* q
        Td -> uTd |* q
        Ct -> uX |* q
        _ -> q

-- Returns binary of integer number
toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (toBin' n)

toBin' 0 = []
toBin' n = (n `mod` 2) : (toBin' (n `div` 2))

-- Add 0 in front of binary n until length equals to len
formatBin len n
    | length n == len = n
    | otherwise = formatBin len (0 : n)

-- Creates list of possibles outcomes for length of list of states
createResList :: Int -> [[Int]]
createResList len = map (formatBin len) (map toBin [0..((2^len)-1)])

-- Creates list of possible results with calcutated probabilities
collapseStates :: States -> T
collapseStates s = map (createRList s) (createResList (length s))

-- Create Result data structure
createRList :: States -> [Int] -> R
createRList s bin = R (foldr (*) 1 (calculateProbs s bin)) bin

-- Calculates probabilities for collapsing QBits
calculateProbs :: States -> [Int] -> [Double]
calculateProbs s bin = zipWith calcP s bin

-- Calculate probability of QBit being |1> or |0> (q1/q0)
calcP :: QBit -> Int -> Double
calcP q b
    | b == 0 = 1 - betanorm2 q
    | otherwise = betanorm2 q

-- Returns True if probablity of R (result) is greater than 0
isRgt0 :: R -> Bool
isRgt0 (R p _) = p > 0

-- Function for multiplying probability in R
mulR :: Double -> R -> R
mulR treeP (R p bit) = R (treeP * p) bit  

l1, l2, l3, l4, l5, l6, l7 :: Level
l1 = Level [Y, Z] False
l2 = Level [H, X] False
l3 = Level [E, H] True
l4 = Level [Ct, Cc] True
l5 = Level [H, H, E] False
l6 = Level [Cc, Cc, Ct] True
l7 = Level [H, H, E] True

c1, c2, c3, c4 :: Circuit
c1 = [Level [] True, Level [H, X] False , Level [] True]
c2 = [Level [H, X] False]
c3 = [l3, l4, l3]
c4 = [l5, l6, l7]

st :: StateTree
st = StateTree 1 [q0, q0, q0] []

rt = RT st []

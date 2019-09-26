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

-- ----------------Structure for stroing resutls

data R = R QBit Double
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

processCircuit :: Circuit -> StateTree -> StateTree 
processCircuit (l:ls) t = -- TODO : tu bude if t = true tak sprav novy Ts (cize vysledky)
    let newStateTree = processLevel l t
    in processCircuit ls newStateTree
processCircuit [] t = t

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

-- Returns probablitiy of Ct being 1
-- if e is not Ct returns -1
probCt1 :: Element -> QBit -> Double
probCt1 e q
    | e == Ct = betanorm2 q
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
        Cc -> uX |* q
        _ -> q


l1, l2, l3, l4 :: Level
l1 = Level [Y, Z] False
l2 = Level [H, X] False
l3 = Level [E, H] False
l4 = Level [Cc, Ct] False

c1, c2, c3 :: Circuit
c1 = [Level [] True, Level [H, X] False , Level [] True]
c2 = [Level [H, X] False]
c3 = [l3, l4, l3]

st1, st2, st3, st4, st5, st6 :: StateTree
st1 = StateTree 1 [q0, q0] [st2, st3]

st2 = StateTree 2 [q0, q0] [st4, st5]

st3 = StateTree 3 [q0, q0] [st6]

st4 = StateTree 4 [q0, q0] []

st5 = StateTree 5 [q0, q0] []

st6 = StateTree 1 [q0, q0] []

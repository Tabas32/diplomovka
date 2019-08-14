import Data.Complex
import QDefinitions
import Data.Map

-- ----------------Struktura pre ukladanie qvantovych obvodov

data Element = X
    | Y
    | Z
    | H
    | S
    | Sd
    | T
    | Td
    | C
    | N
    deriving Show

type LevelGates = [Element]

-- Level :
--     LevelGates : [H1, H2, ... ]
--     measurePoint : T
data Level = Level LevelGates Bool deriving Show

type Circuit = [Level]

-- ----------------Struktura pre ukladanie vysledkov

type Results = [Result]
type Result = Map.empty


-- ----------------Struktura pre ukladanie stavov

type QBit = [[Complex Double]]
type States = [QBit]
type SubTrees = [StateTree]

-- StateTree :
--     probability : P of tree hapening
--     States : states of QBits in one moment
--     SubTrees : possible states of QBits down the circuit
data StateTree = StateTree Double States SubTrees deriving Show

processCircuit :: Circuit -> StateTree -> StateTree 
processCircuit (l:ls) t =
    let newStateTree = processLevel l t
    in processCircuit ls newStateTree
processCircuit [] t = t

-- Aplikuje Level na listy StateTree
processLevel :: Level -> StateTree -> StateTree
processLevel l@(Level g t) st@(StateTree p s []) = StateTree (p+0.1) s []-- tu bude vypocet noveho stromu
processLevel l@(Level g t) st@(StateTree p s subts) = StateTree p s (map (processLevel l) subts)

-- Vytvory novy list pre StateTree pouzitim operacii v levely
calculateStateTree :: Level -> StateTree -> StateTree
--calculateStateTree 


l1, l2 :: Level
l1 = Level [Y, Z] False
l2 = Level [H, X] False

c1, c2 :: Circuit
c1 = [Level [] True, Level [H, X] False , Level [] True]
c2 = [Level [H, X] False]

st1, st2, st3, st4, st5, st6 :: StateTree
st1 = StateTree 1 [q0, q0] [st2, st3]

st2 = StateTree 2 [q0, q0] [st4, st5]

st3 = StateTree 3 [q0, q0] [st6]

st4 = StateTree 4 [q0, q0] []

st5 = StateTree 5 [q0, q0] []

st6 = StateTree 6 [q0, q0] []

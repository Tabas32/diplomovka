import System.IO
import QCircuits
import QDefinitions
import Data.List


main = do
    let l1 = Level [H, E, E ] False
        l2 = Level [Cc, Ct, E] False
        l3 = Level [E, H, E] False
        l4 = Level [Cc, Cc, Ct] False
        l5 = Level [H, E, E] True
        c = [l1, l2, l3, l4, l5]
        st = StateTree 1 [q0, q0, q0] []
        rt = RT st []
    putStrLn "What should be the name of output file:"
    output <- getLine
    let oFilePath = output ++ ".tex"
    putStrLn ("Writing into " ++ oFilePath)
    let processRT = processCircuit c rt
    let tEXTree = makeTEXTree (getStateTree processRT)
    let tEXTs = makeTEXTs (reverse (getTs processRT))
    writeFile oFilePath (makeTEXDoc tEXTree tEXTs)

makeTEXTs :: Ts -> String
makeTEXTs ts = foldr (++) "" (map tToTable ts)

tToTable :: T -> String
tToTable t = 
    "\\begin{tabular}{|"
    ++ (concat $ replicate (length t) "c|")
    ++ "}\n\\hline\n"
    ++ showProbs t ++ " \\\\ \n"
    ++ showBins t ++ " \\\\ \n"
    ++ "\\hline\n"
    ++ "\\end{tabular}\n\n"

showProbs :: T -> String
showProbs t = intercalate " & " (map show (map getProb t))

getProb :: R -> Double
getProb (R p _) = p

showBins:: T -> String
showBins t = intercalate " & " (map getBin t)

getBin :: R -> String
getBin (R _ bin) = concat (map show bin)

makeTEXTree :: StateTree -> String
makeTEXTree (StateTree p s []) = 
        "node[text width=6cm] { \\textbf{" 
        ++ (show p) ++ ":: }"
        ++ (showStates s)
        ++ " }"
makeTEXTree (StateTree p s st) = 
        "node[text width=6cm]{ \\textbf{"
        ++ (show p) ++ ":: }" 
        ++ (showStates s) 
        ++ " } [level distance=3cm]"
        ++ (foldr (++) "" (map wrapAsChild (map makeTEXTree st)))

wrapAsChild :: String -> String
wrapAsChild s = "\n\tchild { " ++ s ++ " }"

showQBit :: QBit -> String
showQBit q = (show q) ++ ","

showStates :: States -> String
showStates s = foldr (++) "" (map showQBit s)

getStateTree :: RT -> StateTree
getStateTree (RT st ts) = st

getTs :: RT -> Ts
getTs (RT _ t) = t

makeTEXDoc :: String -> String -> String
makeTEXDoc tree ts =
    let tEXBegin = "\\documentclass{article}\n\
        \ \\usepackage{tikz}\n\
        \ \\begin{document}\n\
        \ \\begin{tikzpicture}[sibling distance=7cm,\
        \  every node/.style = {shape=rectangle, rounded corners,\
        \    draw, align=center,\
        \    top color=white, bottom color=blue!20}]]\n\
        \  \\"
        treeEnd = ";\n\\end{tikzpicture}\n\n"
        tEXEnd = "\\end{document}"
    in tEXBegin ++ tree ++ treeEnd ++ ts ++ tEXEnd


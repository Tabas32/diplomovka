main = do
    putStrLn "What should be the name of output file:"
    output <- getLine
    putStrLn ("Writing into " ++ output ++ ".tex")

-- TODO: dokoncit zapisovanie do suboru
-- viac info na : http://learnyouahaskell.com/input-and-output

module Main where

import Concordance

main :: IO ()
main = do
    -- Read input files
    errorReport <- readFile "latex-error-report.txt"
    latexSource <- readFile "entropy.tex"

    -- Build concordance
    let concordance = buildConcordance errorReport latexSource

    -- Write output
    writeFile "concordance.txt" (formatConcordanceCSV concordance)

    putStrLn $ "Created concordance with " ++ show (length concordance) ++ " entries"
    putStrLn "Concordance written to concordance.txt"
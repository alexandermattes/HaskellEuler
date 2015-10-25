{-|
    Module      : Main
    Description : Main module with CLI
    Copyright   : (C) Alexander Mattes, 2015
    License     : MIT
    Maintainer  : Alexander Mattes <alexm128@googlemail.com>
    Stability   : experimental
    Portability : portable

    The Main module with all CLI related functions.
-}
module Main
    ( main
    , tp
    ) where


import System.IO (hFlush, stdout)
import System.TimeIt (timeItT)
import Data.Maybe (fromJust, isNothing)
import Text.Printf (printf)

import Solve (ProblemID, minProblemID, maxProblemID, solveProblem, solutions, numberSolvedProblems, tp)
import Verify (isCorrectFor)


-- | The main function with the CLI.
main :: IO ()
main = do
    putStrLn "~~~ Project Euler Solver ~~~"
    putStrLn (show numberSolvedProblems ++ " out of "
              ++ show (maxProblemID-minProblemID+1) ++ " problems have solutions available.")
    let loop = do
            putStr "Please enter problem number (0 for all): "
            hFlush stdout
            n <- readLn :: IO ProblemID
            if n==0 then
                printAllSolutions
            else
                printSolution n
            loop
    loop

-- | Prints all available solutions.
printAllSolutions :: IO ()
printAllSolutions = do
    putStrLn "~~~ Gotta solve em all! ~~~"
    mapM_ (printSolution . fst) solutions

-- | Prints a solution for a given problems.
--   Also checks if the solution is correct and how long the calculation took.
printSolution :: ProblemID -> IO ()
printSolution n = do
    let a = solveProblem n
    if isNothing a then
        putStrLn ("Problem " ++ show n ++ " not solved yet.")
    else do
        _ <- printf "Problem %3d: " n
        let s = fromJust a
        (t,_) <- timeItT  $ printf "%-35s" s
        let c = case s `isCorrectFor` n of
                Nothing    -> "   (unknown)"
                Just True  -> "   (correct)"
                Just False -> " (incorrect)"
        printf "%s time: %6.2fs\n" c t

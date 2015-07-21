{-|
    Module      : Solve
    Description : Solve project euler problems
    Copyright   : (C) Alexander Mattes, 2015
    License     : MIT
    Maintainer  : Alexander Mattes <alexm128@googlemail.com>
    Stability   : experimental
    Portability : portable

    Get solutions for project euler problems.
-}
module Solve
    ( ProblemID
    , Solution
    , minProblemID
    , maxProblemID
    , solveProblem
    , solutions
    , numberSolvedProblems
    , tp
    ) where


import Data.Maybe

import Utils (ProblemID, Solution, time)


-- | Minimal id for available project euler problems.
minProblemID :: ProblemID
minProblemID = 1

-- | Maximal id for available project euler problems.
maxProblemID :: ProblemID
maxProblemID = 522

-- | Returns the solution for a project euler problem.
solveProblem :: ProblemID       -- ^ The problem id.
             -> Maybe Solution  -- ^ The solution, if implemented, otherwise 'Nothing'.
solveProblem n = Nothing

-- | List of solutions for all solved problems.
solutions :: [(ProblemID,Solution)]
solutions = [(n,fromJust s)| n<-[minProblemID..maxProblemID], let s=solveProblem n, isJust s]

-- | The number of solved problems.
numberSolvedProblems :: Int
numberSolvedProblems = length solutions

-- | Helper function to quickly benchmark the time to calculate a problem solution.
--
--   >>> tp 1
--   "123456789"
--   CPU time:   0.00s
tp :: ProblemID -> IO ()
tp = time . showSolution where
    showSolution :: ProblemID -> String
    showSolution n = fromMaybe ("Problem "++ show n ++ " not solved yet") (solveProblem n)
{-|
    Module      : Solutions.Solutions0XX
    Description : Solutions for problems 1-99

    This module contains all implemented solutions for problems with IDs between 1 and 99.
-}
module Solutions.Solutions0XX
    ( solveProblem0XX
    ) where


import Utils (ProblemID, Solution, js)


-- | Solutions for problems with IDs between 1 and 99.
solveProblem0XX :: ProblemID -> Maybe Solution
solveProblem0XX 1 = js 123456789 -- Edit this line
solveProblem0XX 2 = Just "Edit me!" -- Edit this line
-- Add further solutions here
solveProblem0XX _ = Nothing
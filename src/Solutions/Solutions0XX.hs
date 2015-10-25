{-|
    Module      : Solutions.Solutions0XX
    Description : Solutions for problems 1-99

    This module contains all implemented solutions for problems with IDs between 1 and 99.
-}
module Solutions.Solutions0XX
    ( solveProblem0XX
    ) where


import Prelude hiding (sum, product)
import Utils (ProblemID, Solution, js)
import Solutions.Math

-- | Solutions for problems with IDs between 1 and 99.
solveProblem0XX :: ProblemID -> Maybe Solution

solveProblem0XX 1 = js $ sum [3,6..999] + sum [5,10..999] - sum [15,30..999]

solveProblem0XX 2 = js . sum . takeWhile (<4000000) . filter even $ fibs

solveProblem0XX _ = Nothing
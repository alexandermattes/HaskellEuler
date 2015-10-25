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
import ProblemData
import Solutions.Math

-- | Solutions for problems with IDs between 1 and 99.
solveProblem0XX :: ProblemID -> Maybe Solution

solveProblem0XX 1 = js $ sum [3,6..999] + sum [5,10..999] - sum [15,30..999]

solveProblem0XX 2 = js . sum . takeWhile (<4000000) . filter even $ fibs

solveProblem0XX 3 = js . maximum . primefactors $ 600851475143

solveProblem0XX 4 = js . maximum $ [p|x<-[100..999],y<-[x..999], let p=x*y, isPalindrome . show $ p]

solveProblem0XX 5 = js . foldl lcm 1 $ [1..20]

solveProblem0XX 6 = js . abs $ sum [x^2 | x<-[1..100]] - (sum [1..100])^2

solveProblem0XX 7 = js $ primes !! (10001-1)

solveProblem0XX _ = Nothing
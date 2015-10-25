{-|
    Module      : HashSolutions
    Description :
    Copyright   : (C) Alexander Mattes, 2015
    License     : MIT
    Maintainer  : Alexander Mattes <alexm128@googlemail.com>
    Stability   : experimental
    Portability : portable


-}
module HashSolutions
    ( createSolutionHashes
    ) where


import Text.Printf (printf)

import Utils (ProblemID, Solution)
import Verify (hash)


createSolutionHashes :: IO ()
createSolutionHashes = putStrLn . hashSolutions $ solutionList


hashSolutions :: [(ProblemID, Solution)] -> String
hashSolutions =  (++"]") . ('[':) . tail . unlines . map f where
    f :: (ProblemID, Solution) -> String
    f (n, s) = ", (" ++ (printf "%3d" n) ++ ", " ++ (show . hash $ s) ++ ")"


solutionList :: [(ProblemID, Solution)]
solutionList = []
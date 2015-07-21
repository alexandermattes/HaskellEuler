{-|
    Module      : Verify
    Description : Check if a solution is correct
    Copyright   : (C) Alexander Mattes, 2015
    License     : MIT
    Maintainer  : Alexander Mattes <alexm128@googlemail.com>
    Stability   : experimental
    Portability : portable

    Uses hash values to verify offline if a solution is correct.
-}
module Verify
    ( isCorrectFor
    , isCorrectlySolved
    ) where


import Solve (ProblemID, Solution, solveProblem)

import Data.Bits (xor)
import Data.List (foldl')

-- | Type for hashes.
type Hash = Int

-- | Checks a solution via stored hash of the correct solution.
--   Returns 'Nothing' if no hash is stored.
isCorrectFor :: Solution -> ProblemID -> Maybe Bool
isCorrectFor s n = fmap (== hash s) $ lookup n hashedSolutions

-- | Checks if a problem is correctly solved via stored hash of the correct solution.
--   Returns 'Nothing' if no hash is stored.
isCorrectlySolved :: ProblemID -> Maybe Bool
isCorrectlySolved n = case solveProblem n of
                Nothing -> Just False
                Just s  -> isCorrectFor s n

-- | Simple hash function for strings. Variant of 'DJB2'.
hash :: Solution -> Hash
hash = foldl' (\h c -> 33*h `xor` toNumber c) 5381 where
    toNumber :: Char -> Int
    toNumber = (*7754304927747644779) . fromEnum -- random big number to spread hash values

-- | List of hashes for most Solutions.
hashedSolutions :: [(ProblemID,Hash)]
hashedSolutions = [] -- Not filled yet
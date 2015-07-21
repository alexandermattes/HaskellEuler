{-|
    Module      : Utils
    Description : Some helper functions
    Copyright   : (C) Alexander Mattes, 2015
    License     : MIT
    Maintainer  : Alexander Mattes <alexm128@googlemail.com>
    Stability   : experimental
    Portability : portable

    Various helper functions and types.
-}
module Utils
    ( ProblemID
    , Solution
    , time
    , js
    , litFile
    ) where


import Language.Haskell.TH (Lit(StringL), Q, Exp(LitE))
import Language.Haskell.TH.Quote (QuasiQuoter (..), quoteFile)
import System.TimeIt (timeIt)


-- | Type for problem ids. There is no check if a problem with this id actually exists.
type ProblemID = Int

-- | Type for problem solutions.
type Solution = String

-- | Helper function to quickly benchmark a calculation.
--
-- >>> time (isPrime 54651563)
-- True
-- CPU time:   0.00s
time :: (Show a) => a -> IO ()
time = timeIt . print

-- | Shortcut for the often needed 'Just. show'
js :: Show a => a -> Maybe Solution
js = Just . show

-- | Function to import textfiles at compile time via template-haskell.
--
-- > aString = [litFile|path\/to\/text.txt|] :: String
litFile :: QuasiQuoter
litFile = quoteFile lit where
    lit :: QuasiQuoter
    lit = QuasiQuoter
            { quoteExp  = literally
            , quotePat  = error "Shouldn't be used."
            , quoteType = error "Shouldn't be used."
            , quoteDec  = error "Shouldn't be used."
            } where
        literally :: String -> Q Exp
        literally = return . LitE . StringL
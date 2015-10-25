# HaskellEuler #
A small framework to streamline problem solving for projecteuler.net in haskell.

## Usage ##

Preparations

    cabal sandbox init
    cabal update
    cabal install --only-dependencies
    
Add your solutions to the corresponding files in ./src/Solutions/

Build and execute  

    cabal build
    ./dist/build/HaskellEuler/HaskellEuler
    
or run in interpreter to test your solutions

    cabal repl
    *Main> tp 4
    "Problem 4 not solved yet"
    CPU time:   0.00s

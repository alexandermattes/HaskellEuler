# HaskellEuler #
A small framework to streamline problem solving for projecteuler.net in haskell.

## Usage ##

cabal sandbox init
cabal update
cabal install --only-dependencies

./src/Solutions/

cabal build
./dist/build/HaskellEuler/HaskellEuler

cabal repl
*Main> tp 4
"Problem 4 not solved yet"
CPU time:   0.00s

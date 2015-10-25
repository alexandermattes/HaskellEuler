{-|
    Module      : Solutions.Math
    Description : Some mathematical utility functions

    Mathematical algorithms and utility functions
-}
module Solutions.Math where


import Prelude hiding (sum, product)
import Data.List hiding (sum, product)




{-
 ____            _          
| __ )  __ _ ___(_) ___ ___ 
|  _ \ / _` / __| |/ __/ __|
| |_) | (_| \__ \ | (__\__ \
|____/ \__,_|___/_|\___|___/
-}

sum :: Num a => [a] -> a
sum = foldl' (+) 0

product :: Num a => [a] -> a
product = foldl' (*) 1

fac :: Integer -> Integer
fac n = product [1..n]



{-
 ____                                            
/ ___|  ___  __ _ _   _  ___ _ __   ___ ___  ___ 
\___ \ / _ \/ _` | | | |/ _ \ '_ \ / __/ _ \/ __|
 ___) |  __/ (_| | |_| |  __/ | | | (_|  __/\__ \
|____/ \___|\__, |\__,_|\___|_| |_|\___\___||___/
               |_|   
-}

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Integer
fib n = fibs !! n


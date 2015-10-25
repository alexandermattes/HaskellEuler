{-|
    Module      : Solutions.Math
    Description : Some mathematical utility functions

    Mathematical algorithms and utility functions
-}
module Solutions.Math where


import Prelude hiding (sum, product)
import Data.List hiding (sum, product)



{-
  ___  _   _
 / _ \| |_| |__   ___ _ __
| | | | __| '_ \ / _ \ '__|
| |_| | |_| | | |  __/ |
 \___/ \__|_| |_|\___|_|
-}

binaryExpansion :: Integer -> [Integer] -- First Element is LSB
binaryExpansion 0 = [0]
binaryExpansion 1 = [1]
binaryExpansion n = let (n2,bit) = n `divMod` 2 in bit : binaryExpansion n2

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l



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

pow :: Integer -> Integer -> Integer
pow b e = helper b (binaryExpansion e) 1 where
    helper b [] res = res
    helper b (x:xs) res = (if x==1 then (*b) else id) $ q*q where
        q = (helper b xs res)

powMod :: Integer -> Integer -> Integer -> Integer
powMod b e m = helper b (binaryExpansion e) 1 where
    helper b [] res = res
    helper b (0:xs) res = (`mod` m) $ q*q where
        q = (helper b xs res)
    helper b (1:xs) res = (`mod` m) . (*b) . (`mod` m) $ q*q  where
        q = (helper b xs res)

divideMax :: Integer -> Integer -> (Integer,Integer)
-- divides n by d as often as possible and returns (rest,times)
divideMax n d = case divMod n d of
    (t,0) -> let (m,o) = (divideMax t d) in (m,o+1)
    _     -> (n,0)



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



{-
 ____       _
|  _ \ _ __(_)_ __ ___   ___  ___
| |_) | '__| | '_ ` _ \ / _ \/ __|
|  __/| |  | | | | | | |  __/\__ \
|_|   |_|  |_|_| |_| |_|\___||___/
-}

millerRabinSingle :: Integer -> Integer -> Bool
millerRabinSingle n a
    | n `rem` a == 0 = n == a
    | otherwise = (head series == 1) || (n-1) `elem` series where
        (d,j) = divideMax (n-1) 2
        series = take (fromIntegral j+1) . help $ (powMod a d n) where
            help x = x : help (powMod x 2 n)

millerRabin :: Integer -> [Integer] -> Bool
millerRabin n list = and . map (millerRabinSingle n) $ list

isPrime :: Integer -> Bool
isPrime n
    | n <= firstPrimesMax = n > 1 && ((==n). head . dropWhile (<n) $ firstPrimes) --could use binary search instead
    | or . map (\x -> n `rem` x == 0) $ firstPrimes = False
    | n < 1373653         = millerRabin n [2,3]
    | n < 9080191         = millerRabin n [31,73]
    | n < 4759123141      = millerRabin n [2,7,61]
    | n < 2152302898747   = millerRabin n [2,3,5,7,11]
    | n < 3474749660383   = millerRabin n [2,3,5,7,11,13]
    | n < 341550071728321 = millerRabin n [2,3,5,7,11,13,17]
    | otherwise = millerRabin n [2..min (n-1) 2*(floor.log.fromIntegral$n)^2]
    where
        firstPrimes =
            [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83
            ,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181
            ,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283
            ,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409
            ,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523
            ,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647
            ,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773
            ,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911
            ,919,929,937,941,947,953,967,971,977,983,991,997] :: [Integer] -- better: use bitarray
        firstPrimesMax = 997 :: Integer

primes :: [Integer] -- rather slow, should be done with a good sieve
primes = 2:(filter isPrime [3,5..])

primesUntil :: Integer -> [Integer]
primesUntil n = takeWhile ((>=)n) primes

nextPrime :: Integer -> Integer
nextPrime n = head . filter isPrime $ [n..]

previousPrime :: Integer -> Integer
previousPrime n = head . filter isPrime $ [n,n-1..0]

primefactors :: Integer -> [Integer]
primefactors n
    | n < 1 = error "primefactors not defined on non-positive numbers"
    | isPrime n = [n]
    | otherwise = factorWith n (primesUntil n) where
        factorWith 1 _ = []
        factorWith n [] = [n]
        factorWith n (f:fs)
            | f^2 > n = [n]
            | t == 0  = factorWith r fs
            | otherwise = newFactor ++ if isPrime r
                then [r] else factorWith r fs where
                    (r,t) = divideMax n f
                    newFactor = (replicate (fromIntegral t) f)


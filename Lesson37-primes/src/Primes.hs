module Primes where

upperBound = 25000

sieve :: [Int] -> [Int]
sieve [] = []
sieve (this:rest) = this : sieve noFactors
    where
        noFactors = filter (not . (==0) . (`mod` this)) rest

primes :: [Int]
primes = sieve [2 .. upperBound]

isPrime :: Int -> Maybe Bool
isPrime n
    | n < 2 = Nothing
    | n > length primes = Nothing
    | otherwise = Just (n `elem` primes)


unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) =
    if n `mod` next == 0
    then next:unsafePrimeFactors (n `div` next) (next:primes)
    else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n
    | n < 2 = Nothing
    | n >= length primes = Nothing
    | otherwise = Just (unsafePrimeFactors n primesLessThanN)
    where
        primesLessThanN = filter (<= n) primes

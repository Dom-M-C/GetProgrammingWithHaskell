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

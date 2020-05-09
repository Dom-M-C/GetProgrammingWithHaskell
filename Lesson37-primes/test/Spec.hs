import Test.QuickCheck
import Primes
import Data.Maybe

qcArgs = stdArgs { maxSuccess = 10000 }

prop_validPrimesOnly val =
    if val < 2 || val > length primes
    then result == Nothing
    else isJust result
    where
        result = isPrime val

prop_primesArePrime val =
    if result == Just True
    then (length $ divisors val) == 0
    else True
    where
        result = isPrime val

prop_nonPrimesAreComposite val =
    if result == Just False
    then (length $ divisors val) > 0
    else True
    where
        result = isPrime val

divisors val = filter ((==0) . (val `mod`)) [2 .. (val -  1)]

prop_factorsMakeOriginal val =
    if result == Nothing
    then True
    else product (fromJust result) == val
    where
        result = primeFactors val

prop_allFactorsPrime val =
    if result == Nothing
    then True
    else all (\x -> isPrime x == Just True) $ fromJust result
    where
        result = primeFactors val



main :: IO ()
main = do
    quickCheckWith qcArgs prop_validPrimesOnly
    quickCheckWith qcArgs prop_primesArePrime
    quickCheckWith qcArgs prop_nonPrimesAreComposite
    quickCheckWith qcArgs prop_factorsMakeOriginal
    quickCheckWith qcArgs prop_allFactorsPrime

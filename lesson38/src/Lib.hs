{-# LANGUAGE OverloadedStrings #-}

module Lib
(   isPrime
,   displayResult
) where

import qualified Data.Text as T


myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n-1) (tail xs)

myTakePm :: Int -> [a] -> [a]
myTakePm 0 _ = []
myTakePm n (x:xs) = x: myTakePm (n-1) xs

textHead :: T.Text -> T.Text
textHead x = case T.uncons x of
    Just (y, _) -> T.pack [y]
    Nothing -> ""

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

mySaferTake :: Int -> Maybe [a] -> Maybe [a]
mySaferTake 0 _ = Just []
mySaferTake n (Just xs) = (:) <$> maybeHead xs
    <*> mySaferTake (n-1) (Just (tail xs))

data Nature = Good | Bad deriving Show

eitherGoodOrBad :: Int -> Either Nature Nature
eitherGoodOrBad n
    | n `mod` 2 == 0 = Left Good
    | otherwise = Left Bad

eitherHead :: [a] -> Either String a
eitherHead [] = Left "empty list"
eitherHead (x:_) = Right x

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

primes = [2,3,5,7]

maxN = 10

data PrimeError = TooBig | Invalid

instance Show PrimeError where
    show TooBig = "number too big to calculate prime"
    show Invalid = "not a candidate"

isPrime :: Int -> Either PrimeError Bool
isPrime n
    | n < 2 = Left Invalid
    | n > maxN = Left TooBig
    | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Left err) = show err
displayResult (Right True) = "prime"
displayResult (Right False) = "nope - composite number"

{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"


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



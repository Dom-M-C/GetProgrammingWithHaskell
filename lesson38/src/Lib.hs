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

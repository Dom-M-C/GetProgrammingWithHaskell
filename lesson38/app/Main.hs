module Main where

import Lib

main :: IO ()
main = do
    print "gimme a number"
    n <- read <$> getLine
    let result = isPrime n
    print (displayResult result)

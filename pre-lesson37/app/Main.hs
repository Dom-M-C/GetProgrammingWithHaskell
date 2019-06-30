module Main where


import Test.QuickCheck
import Test.QuickCheck.Instances
import Lib
import Data.Char(isPunctuation)

import qualified Chapter26 as C
import Data.Text as T

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
    then putStrLn passStatement
    else putStrLn failStatement




main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
    quickCheck prop_reverseInvariant
    putStrLn "done"


prop_punctuationInvariant t = preprocess t == preprocess noPuncText
    where
        noPuncText = T.filter (not . isPunctuation) t


prop_reverseInvariant t = isPalindrome t == isPalindrome (T.reverse t)

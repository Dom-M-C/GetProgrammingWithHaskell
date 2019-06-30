module Palindrome
    (   isPalindrome
    ) where

import Data.Char (toLower, isSpace, isPunctuation)

stripWhitespace :: String -> String
stripWhitespace = filter (not . isSpace)

stripPunctuation :: String -> String
stripPunctuation = filter (not . isPunctuation)

toLowerCase :: String -> String
toLowerCase = map toLower

preprocess :: String -> String
preprocess = stripWhitespace . stripPunctuation . toLowerCase

isPalindrome :: String -> Bool
isPalindrome s = washedText == reverse washedText
    where washedText = preprocess s



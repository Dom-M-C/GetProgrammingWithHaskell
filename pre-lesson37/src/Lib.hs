module Lib where

import Data.Char(isPunctuation)
import qualified Data.Text as T

isPalindrome :: T.Text -> Bool
isPalindrome t = cleanText == T.reverse cleanText
    where
        cleanText = preprocess t

preprocess :: T.Text -> T.Text
preprocess = T.filter (not . isPunctuation)




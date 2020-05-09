module Chapter34 where

import Palindrome

head :: Monoid a => [a] -> a
head (x:_) = x
head [] = mempty



example :: [[Int]]
example = []




main :: IO ()
main = putStrLn "word please"
    >> getLine
    >>= (\ln ->
        if isPalindrome ln
        then putStrLn "palindrome"
        else putStrLn "nope" )


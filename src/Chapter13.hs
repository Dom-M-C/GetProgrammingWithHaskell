module Chapter13 where

import Data.List

data Icecream = Vanilla | Chocolate deriving(Eq, Show, Ord, Enum, Bounded)

sortedIcecream = sort [Chocolate, Vanilla, Chocolate, Vanilla]


cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc x
    | x == maxBound = minBound
    | otherwise = succ x

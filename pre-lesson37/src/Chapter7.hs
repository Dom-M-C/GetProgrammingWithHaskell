module Chapter7 where

dmcTake :: Int -> [a] -> [a]
dmcTake 0 _ = []
dmcTake n (x:xs) = x : dmcTake (n-1) xs

dmcCycle :: [a] -> [a]
dmcCycle xs = xs ++ dmcCycle xs


dmcGcd :: Int -> Int -> Int
dmcGcd x y = if remain == 0
    then y
    else dmcGcd y remain
    where
        remain = x `mod` y


--todo
dmcGcd' :: Int -> Int -> Int
dmcGcd' x 1 = x
dmcGcd' 0 x = 0
dmcGcd' x y = undefined



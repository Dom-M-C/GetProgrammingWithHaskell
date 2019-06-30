module Chapter6 where


partition x = (zip . cycle) [1..x]


dmcRepeat :: a -> [a]
dmcRepeat x = cycle (x:[])

subseq :: Int -> Int -> [a] -> [a]
subseq start end xs = sub xs
    where sub = take end . drop start

inFirstHalf :: (Eq a) => a -> [a] -> Bool
inFirstHalf x xs = elem x firstHalf
    where
        firstHalf = take halfLen xs
        halfLen = div (length xs) 2


module Chapter8 where


dmcLength [] = 0
dmcLength (x:xs) = 1 + dmcLength xs
--dmcLength xs = 1 + dmcLength (tail xs)

dmcTake [] _ = []
dmcTake _ 0 = []
dmcTake (x:xs) y = x : dmcTake xs ((abs y) - 1)

dmcCycle xs = xs ++ dmcCycle xs


dmcAckerman 0 n = n + 1
dmcAckerman m 0 = dmcAckerman (m - 1) 1
dmcAckerman m n = dmcAckerman (m - 1) $ dmcAckerman m (n-1)

dmcCollatz :: Int -> Int
dmcCollatz 1 = 0
dmcCollatz n
    | even n = 1 + dmcCollatz (n `div` 2)
    | odd n = 1 + dmcCollatz (n * 3 + 1)

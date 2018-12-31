module Chapter9 where


dmcMap _ [] = []
dmcMap f (x:xs) = f x:dmcMap f xs

dmcFilter :: (a -> Bool) -> [a] -> [a]
dmcFilter _ [] = []
dmcFilter pred (x:xs)
    | pred x = x : dmcFilter pred xs
    | otherwise = dmcFilter pred xs

dmcFoldl :: (a -> a -> a) -> a -> [a] -> a
dmcFoldl f i [x] = f i x
dmcFoldl f i (x:xs) = dmcFoldl f (f i x) xs

dmcFoldl' f i [] = i
dmcFoldl' f i (x:xs) = dmcFoldl' f newInit xs
    where newInit = f i x

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct xs = dmcFoldl (*) 1 xs

dmcElem _ [] = []
dmcElem n xs = undefined --use filter and length

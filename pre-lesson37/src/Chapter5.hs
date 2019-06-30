module Chapter5 where

subtract2 = flip (-) 2

subtract2' x = (-) x 2

subtract2'' = (\x -> x - 2)

ifEven f x = if even x
    then f x
    else x

ifEvenInc = ifEven (+1)
ifEvenDouble = ifEven (*2)
ifEvenSquare = ifEven (^2)

binaryPartialApplication f x = (\y -> f y x)

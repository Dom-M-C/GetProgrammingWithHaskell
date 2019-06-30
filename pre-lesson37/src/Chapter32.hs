module Chapter32 where

import Control.Monad
import Data.Char

powersOfTwo :: (Num b, Enum b) => b -> [b]
powersOfTwo n = do
    value <- [1 .. n]
    return (value^2)

powersOfTwoAndThree n = do
    value <- [1..n]
    let powersOfTwo = 2^value
    let powersOfThree = 3^value
    return (powersOfTwo, powersOfThree)

evensAndOdds n = do
    evens <- [2,4..n]
    odds <- [1,3..n]
    return (evens, odds)

odds n = do
    value <- [1..n]
    guard(even value)
    return value

guardFilter f xs = do
    x <- xs
    myGuard(f x)
    return x


myGuard b = do
    if b then return () else error "mzero"

powersOfTwoComprehension n = [2^value | value <- [1..n]]

powersOfTwoAndThreeComprehension n = [(powersOfTwo, powersOfThree)
    | value <- [1..n]
    , let powersOfTwo = 2^value
    , let powersOfThree = 3^value
    ]

colours = ["brown", "blue", "pink", "orange"]

mrColours = [ "Mr. " <> col
    | colour <- colours
    , let col = (\(h:rest) -> (toUpper h) : rest) colour
    ]

months = [[january, february,march,april,may,june,july,august,september,october,november,december]
    | let january = [1..31]
    , let february = [1..28]
    , let march = [1..31]
    , let april = [1..30]
    , let may = [1..31]
    , let june = [1..30]
    , let july = [1..31]
    , let august = [1..31]
    , let september = [1..30]
    , let october = [1..31]
    , let november = [1..30]
    , let december = [1..31]]

monthsDo :: Monad m => m [[Int]]
monthsDo = do
    let january = [1..31]
    let february = [1..28]
    let march = [1..31]
    let april = [1..30]
    let may = [1..31]
    let june = [1..30]
    let july = [1..31]
    let august = [1..31]
    let september = [1..30]
    let october = [1..31]
    let november = [1..30]
    let december = [1..31]
    return [january, february,march,april,may,june,july,august,september,october,november,december]


--monthsDont :: Monad m => m [[Int]]
monthsDont = [1..31]
    >>= (\january    -> [1..28]
    >>= (\february   -> [1..31]
    >>= (\march      -> [1..30]
    >>= (\april      -> [1..31]
    >>= (\may        -> [1..30]
    >>= (\june       -> [1..31]
    >>= (\july       -> [1..31]
    >>= (\august     -> [1..30]
    >>= (\september  -> [1..31]
    >>= (\october    -> [1..30]
    >>= (\november   -> [1..31]
    >>= (\december   ->
    return [january, february,march,april,may,june,july,august,september,october,november,december]
    ))))))))))))





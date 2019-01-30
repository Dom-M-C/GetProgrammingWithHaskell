module Chapter22 where

import System.Environment
import Control.Monad

mainArgs :: IO ()
mainArgs = do
    args <- getArgs
    mapM_ putStrLn args


mainGetLine n = do
    lines <- mapM (\_ -> getLine) [1..n]
    mapM_ print lines

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
        then read (head args)
        else 0 :: Int
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n f = mapM (\_ -> f) [1..n]

sumLazyMain :: IO ()
sumLazyMain = do
    userInput <- getContents
    mapM_ print userInput


sampleData = ['6','2','\n','2','1','\n']

toInts :: String -> [Int]
toInts = map read . lines


sumOfSquares :: [Int] -> Int
sumOfSquares = sum . map (^2)

toIntsMain :: IO ()
toIntsMain = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sumOfSquares numbers)

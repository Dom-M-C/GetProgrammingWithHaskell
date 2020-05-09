module Main where

import Prelude
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

main :: IO ()
main = print "hello world"

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3, True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]

qcArray :: UArray Int Bool
qcArray = array (0, 4) [(1, True), (2, True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) []


beansInBuckets' :: UArray Int Int
beansInBuckets' = array (0, 3) $ zip [1 .. 3] $ cycle [0]

updatedBiB = beansInBuckets' // [(1,5), (3,6)]

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1
    stArray <- newArray (0, end) 0
    forM_ [0 .. end] $
        \i -> do
            let val = vals !! i
            writeArray stArray i val
    return stArray


listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals


sortData :: UArray Int Int
sortData = listArray (0, 5000) [1..5001]

bubbleSort' :: UArray Int Int -> UArray Int Int
bubbleSort' unsortedArray = runSTUArray $ do
    stArray <- thaw unsortedArray
    let end = (snd . bounds) unsortedArray
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - i)] $ \j -> do
            current <- readArray stArray j
            next <- readArray stArray (j + 1)
            let outOfOrder = current > next
            when outOfOrder $ do
                writeArray stArray j next
                writeArray stArray (j + 1) current
    return stArray

type BoolArray = UArray Int Bool

crossover :: (BoolArray, BoolArray) -> Int -> BoolArray
crossover (top, bottom) cutoff = runSTUArray $ do
    -- | endF top /= endF bottom = error "arrays must be equal length"
    -- | otherwise = runSTUArray $ do
    stTop <- thaw top
    let end = endF top
    forM_ [cutoff .. end] $ \i -> do
        let botVal = bottom ! i
        writeArray stTop i botVal
    return stTop
    where
        endF = (snd . bounds)


topArray :: BoolArray
topArray = array (0,4) $ zip [0 .. 4] $ cycle [True]

bottomArray :: BoolArray
bottomArray = array (0,4) []

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros array = runSTUArray $ do
    stArray <- thaw array
    let end = (snd . bounds) array
    forM [0 .. end] $ \i -> do
        current <- readArray stArray i
        let currentIsZero = current == 0
        when currentIsZero $ do
            writeArray stArray i (-1)
    return stArray

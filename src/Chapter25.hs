module Chapter25 where

import System.Environment
import System.Random
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello!"


bcToInt :: B.ByteString -> Int
bcToInt = read . BC.unpack

lovecraft :: IO BC.ByteString
lovecraft = BC.readFile "../lovecraft.jpg"


mainLove :: IO ()
mainLove = do
    args <- getArgs
    let fileName = head args
    let glitchedFileName = mconcat ["glitched_", fileName]
    image <- BC.readFile fileName
    glitched <- foldM (\bytes func -> func bytes) image sortList
    BC.writeFile glitchedFileName glitched
    print "done"
    where
        sortList = [randomReplaceByte, randomReplaceByte, randomSortSection, randomSortSection, randomReplaceByte]

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where
        safeInt = int `mod` 256

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
    where
        (before, rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1, bytesLength)
    charVal <- randomRIO (0, 255)
    return $ replaceByte location charVal bytes

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes =  mconcat [before, changed, after]
    where
        (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest
        changed = BC.reverse $ BC.sort target

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 7
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return $ sortSection start sectionSize bytes

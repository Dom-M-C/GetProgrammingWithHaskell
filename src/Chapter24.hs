module Chapter24 where

import System.IO
import System.Environment


openMain :: IO ()
openMain = do
    myFile <- openFile "hello.txt" ReadMode
    hClose myFile
    putStrLn "done!"

rwStdoutMain :: IO ()
rwStdoutMain = do
    helloFile <- openFile "hello.txt" ReadMode
    firstLine <- hGetLine helloFile
    putStrLn firstLine
    secondLine <- hGetLine helloFile
    goodbyeFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyeFile secondLine
    hClose helloFile
    hClose goodbyeFile
    putStrLn "done!"

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
    where
        charCount = length input
        wordCount = (length . words) input
        lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) = unwords
    [   "chars: ", show cc
    ,   "; words: ", show wc
    ,   "; lines: ", show lc]

describeFile :: IO ()
describeFile = do
    args <- getArgs
    let fileName = head args
    input <- readFile fileName
    let summary = (countsText . getCounts) input
    appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
    putStrLn summary

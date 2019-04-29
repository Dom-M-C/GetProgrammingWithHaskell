{-# LANGUAGE OverloadedStrings #-}

module Chapter23 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

aWord :: T.Text
aWord = "Cheese"


sampleInput :: T.Text
sampleInput = "this\nis\ninput"

someText :: T.Text
someText = "Some\ntext for\t you"

--do QC 23.3

myLines = T.splitOn "\n"

myUnlines = T.intercalate "\n"

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: T.Text -> T.Text -> T.Text
highlight query fulltext = T.intercalate highlighted pieces
    where
        highlighted = mconcat ["\t{", query, "}\t"]
        pieces = T.splitOn query fulltext

sansMain = do
    TIO.putStrLn (highlight dharma bgText)

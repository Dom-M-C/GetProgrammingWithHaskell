module Chapter26 where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

import Data.Maybe

type Author = T.Text
type Title = T.Text

data Book = Book
    {   author :: Author
    ,   title :: Title } deriving Show

type Html = T.Text

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat
    [   "<html>\n"
    ,   "<head><title>books</title>"
    ,   "<meta charset='utf-8'>"
    ,   "<head>\n"
    ,   "<body>\n"
    ,   booksHtml
    ,   "\n</body>\n"
    ,   "</html>"
    ]
    where
        booksHtml = mconcat $ map bookToHtml books

bookToHtml :: Book -> Html
bookToHtml book = mconcat
    [   "<p>\n"
    ,   titleInTags
    ,   authorInTags
    ,   "</p>\n"
    ]
    where
        titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
        authorInTags = mconcat ["<em>", author book, "</em>\n"]

book1 :: Book
book1 = Book
    {   title = "The Conspiracy Against the Human Race"
    ,   author = "Ligotti, Thomas" }

book2 :: Book
book2 = Book
    {   title = "A Short History of Decay"
    ,   author = "Cioran, Emil" }

book3 :: Book
book3 = Book
    {   title = "The Tastes of Eros"
    ,   author = "Bataille, Georges" }

myBooks :: [Book]
myBooks =  [book1,book2,book3]

mainBooks :: IO ()
mainBooks = TIO.writeFile "books.html" $ booksToHtml myBooks

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader raw = B.take leaderLength raw

leaderLength :: Int
leaderLength = 24

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength lead = rawToInt $ B.take 5 lead

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
    where
        recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream
    | marcStream == B.empty = []
    | otherwise = next : allRecords text
    where
        (next, text) = nextAndRest marcStream

marcMain :: IO ()
marcMain = do
    marcData <- B.readFile "sample.mrc"
    let marcRecords = allRecords marcData
    print (length marcRecords)

type MarcDirectoryRaw = B.ByteString







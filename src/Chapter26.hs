module Chapter26 where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

import Data.Maybe
import Control.Monad

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

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader raw = B.take leaderLength raw

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

type MarcDirectoryRaw = B.ByteString

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress = rawToInt . B.take 5 . B.drop 12

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take (getDirectoryLength record) $ B.drop leaderLength record


type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory dir
    | dir == B.empty = []
    | otherwise = nextEntry : splitDirectory rest
    where
        (nextEntry, rest) = B.splitAt dirEntryLength dir

data FieldMetadata = FieldMetadata
    { tag           :: T.Text
    , fieldLength   :: Int
    , fieldStart    :: Int } deriving Show

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
    where
        (theTag, rest) = B.splitAt 3 entry
        textTag = E.decodeUtf8 theTag
        (rawLength, rawStart) = B.splitAt 4 rest
        theLength = rawToInt rawLength
        theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record meta = E.decodeUtf8 byteStringValue
    where
        recordLength = getRecordLength record
        baseAddress = getBaseAddress record
        baseRecord = B.drop baseAddress record
        baseAtEntry = B.drop (fieldStart meta) baseRecord
        byteStringValue = B.take (fieldLength meta) baseAtEntry


marcMain :: IO ()
marcMain = do
    marcData <- B.readFile "sample.mrc"
    let marcRecords = allRecords marcData
    --let all = mconcat $ map getDirectory marcRecords
    --mapM_ print (splitDirectory base)
    let first = splitDirectory . getDirectory $ marcRecords !! 0
    let meta = getFieldMetadata first
    mapM_ print meta

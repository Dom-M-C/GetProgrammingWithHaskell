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
    let first = getMetadata $ marcRecords !! 0

    mapM_ print first

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

type Tag = T.Text
type Subfield = Char

titleTag :: Tag
titleTag = "245"

titleSubfield :: Subfield
titleSubfield = 'a'

authorTag :: Tag
authorTag = "100"

authorSubfield :: Subfield
authorSubfield = 'a'

getMetadata :: MarcRecordRaw -> [FieldMetadata]
getMetadata = getFieldMetadata . splitDirectory . getDirectory

lookupFieldMetadata :: Tag -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record
    | length result > 0 = Just (head result)
    | otherwise = Nothing
    where
        result = filter ((==aTag) . tag) $ getMetadata record

type Field = T.Text

lookupSubfield :: Maybe FieldMetadata -> Subfield -> MarcRecordRaw -> Maybe Field
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just meta) subfield record
    | results == [] = Nothing
    | otherwise = Just ((T.drop 1 . head) results)
    where
        rawField = getTextField record meta
        subfields = T.split (== fieldDelimiter) rawField
        results = filter ((== subfield) . T.head) subfields

lookupValue :: Tag -> Subfield -> MarcRecordRaw -> Maybe Field
lookupValue aTag subfield record = lookupSubfield meta subfield record
    where
        meta = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs stream = zip titles authors
    where
        records = allRecords stream
        titles = map lookupTitle records
        authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map makeBook justPairs
    where
        makeBook (title, author) = Book { title = fromJust title, author = fromJust author }
        justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . (take n) . marcToPairs

main :: IO ()
main = do
    marcData <- B.readFile "sample.mrc"
    let marcRecords = allRecords marcData
    --let num = (length marcRecords)
    let processed = processRecords 100 marcData
    TIO.writeFile "books.html" processed

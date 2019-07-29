module Main where

import Control.Monad
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

main :: IO ()
main = print "hi"


data Book = Book
    {   title :: T.Text
    ,   author :: T.Text
    ,   year :: Int
    } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book "Will Kurt" "Learn Haskell" 2017

jsonBook = encode myBook

decodedBook :: Maybe Book
decodedBook = decode jsonBook

wrongJson :: BC.ByteString
wrongJson = "{\"writer\":\"\"}"

data ErrorMessage = ErrorMessage
    {   message :: T.Text
    ,   errorCode :: Int
    } deriving Show

instance FromJSON ErrorMessage where
    parseJSON (Object v) = ErrorMessage
        <$> v .: "message"
        <*> v .: "error"

data Name = Name
    {   firstName :: T.Text
    ,   lastName :: T.Text
    } deriving Show

instance FromJSON Name where
    parseJSON (Object v) = Name
        <$> v .: "firstName"
        <*> v .: "lastName"

exampleMessage :: Maybe T.Text
exampleMessage = Just "oops"

exampleError :: Maybe Int
exampleError = Just 123

sampleError :: Maybe ErrorMessage
sampleError = ErrorMessage
    <$> exampleMessage
    <*> exampleError

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage msg errCode) =
        object  [   "message" .= msg
                ,   "error" .= errCode
                ]

jsonErrorMessage :: Maybe BC.ByteString
jsonErrorMessage = encode <$> sampleError

data NOAAResult = NOAAResult
    {   uid :: T.Text
    ,   mindate :: T.Text
    ,   maxdate :: T.Text
    ,   name :: T.Text
    ,   datacoverage :: T.Text
    ,   resultId :: T.Text
    }   deriving Show

instance FromJSON NOAAResult where
    parseJSON (Object v) = NOAAResult
        <$> v .: "uid"
        <*> v .: "mindate"
        <*> v .: "maxdate"
        <*> v .: "name"
        <*> v .: "datacoverage"
        <*> v .: "id"

data Resultset = Resultset
    {   offset :: Int
    ,   count :: Int
    ,   limit :: Int
    } deriving (Show, Generic)
instance FromJSON Resultset

data Metadata = Metadata
    {   resultset :: Resultset
    } deriving (Show, Generic)
instance FromJSON Metadata

data NOAAResponse = NOAAResponse
    {   metadata :: Metadata
    ,   results :: [Results]
    } deriving (Show, Generic)
instance FromJSON NOAAResponse


module Main where

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

andBackAgain :: Maybe ErrorMessage
andBackAgain = decode <$> jsonErrorMessage

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

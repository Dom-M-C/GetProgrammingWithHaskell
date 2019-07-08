module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import Network.HTTP.Simple

import Lib

main :: IO ()
main = someFunc



baseUrl endpoint = B.concat ["https://", endpoint]

token :: BC.ByteString
token = "zCtSqkkpdOnIWKEcUjiGdrOXvTZAcQuP"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

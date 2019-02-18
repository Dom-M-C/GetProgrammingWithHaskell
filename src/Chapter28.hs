module Chapter28 where

import qualified Data.Map as Map
import Data.Maybe

type LatLong = (Double, Double)

locationDb :: Map.Map String LatLong
locationDb = Map.fromList
    [   ("Arkham", (42.6054, -70.7829))
    ,   ("Innsmouth", (42.8250, -70.8150))
    ,   ("Carcosa", (29.9714, -90.7694))
    ,   ("New York", (40.7776, -73.9691))]

toRadians :: Double -> Double
toRadians deg = (deg * pi) / 180

latLongToRads :: LatLong -> LatLong
latLongToRads (lat, long) = (rlat, rlong)
    where
        rlat = toRadians lat
        rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine co1 co2 = earthRadius * c
    where
        earthRadius = 3961.0
        (rlat1, rlong1) = latLongToRads co1
        (rlat2, rlong2) = latLongToRads co2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat / 2))^2 + cos rlat1 * cos rlat2 * (sin (dlong / 2))^2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "city not found"
printDistance (Just d) = putStrLn (show d <> " miles")

distanceFromNy = haversine <$> (Map.lookup "New York" locationDb) <*> Just (0,0)

startingCity = Map.lookup "Carcosa" locationDb
destCity = Map.lookup "Innsmouth" locationDb

carcosaToInnsmouth = haversine <$> startingCity <*> destCity

mainDistance = do
    putStrLn "starting city?"
    start <- getLine
    let startCity = Map.lookup start locationDb
    putStrLn "destination?"
    dest <- getLine
    let destination = Map.lookup dest locationDb
    let distance = haversine <$> startCity <*> destination
    printDistance distance

readInt :: IO Int
readInt = read <$> getLine

minOfThree i j = min i . min j

minOfIO :: IO Int
minOfIO = minOfThree <$> readInt <*> readInt <*> readInt

maybeOfthree = minOfThree <$> Just 10 <*> Just 3 <*> Just 6

data User = User
    {   name :: String
    ,   gamerId :: Int
    ,   score :: Int
    } deriving Show

serverName :: Maybe String
serverName = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

justSue = User <$> serverName <*> serverGamerId <*> serverScore

mainUser :: IO ()
mainUser = do
    putStrLn "Enter name, gameId, score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user

getLatLong :: IO LatLong
getLatLong = do
    putStrLn "latitude?"
    lat <- read <$> getLine
    putStrLn "longitude?"
    long <- read <$> getLine
    return (lat, long)

haversineNoApp = do
    latLong1 <- getLatLong
    latLong2 <- getLatLong
    let res = haversine latLong1 latLong2
    return res

haversineIO = haversine <$> getLatLong <*> getLatLong



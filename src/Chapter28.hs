module Chapter28 where

import qualified Data.Map as Map

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






module TimeSeries where

import qualified Data.Map as Map
import Data.List
import Data.Semigroup
import Data.Maybe

file1 :: [(Int, Double)]
file1 =
    [(1,200.1),(2,222.2),(3,195.0)
    ,(4,197.7),(5,192.1),(6,199.0)
    ,(9,190.3),(10,201.0),(12,200.0)]

file2 :: [(Int, Double)]
file2 =
    [(11,197.2),(12,201.5),(13,196.2)
    ,(14,192.1),(15,195.5),(16,198.8)
    ,(18,192.0),(20,199.1)]

file3 :: [(Int, Double)]
file3 =
    [(10,201.2),(11,201.6),(12,205.5)
    ,(13,201.5),(14,203.5),(17,210.5)
    ,(24,215.1),(25,218.7)]

file4 :: [(Int, Double)]
file4 =
    [(26,219.8),(27,220.5),(28,223.8)
    ,(29,222.8),(30,223.8),(31,225.9)
    ,(32,222.3),(33,224.8),(34,230.8)
    ,(35,229.3),(36,233.3)]


data TS a = TS [Int] [Maybe a] deriving (Eq)


createTs :: [Int] -> [a] -> TS a
createTs keys values =
    let
        completeKeys = [minimum keys..maximum keys]
        timeValMap = Map.fromList $ zip keys values
        completeValues = map (\v -> Map.lookup v timeValMap) completeKeys
    in
        TS completeKeys completeValues

fileToTs :: [(Int, a)] -> TS a
fileToTs kvPairs =
    let
        (keys, values) = unzip kvPairs
    in
        createTs keys values
--bfgfdgdfg
showKvPair :: Show a => Int -> (Maybe a) -> String
showKvPair k (Just val) = mconcat [show k, "\t|\t", show val, "\n"]
showKvPair k Nothing = mconcat [show k, "\t|\tNA\n"]

instance Show a => Show (TS a) where
    show (TS keys vals) = mconcat rows
        where rows = zipWith showKvPair keys vals


insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, (Just val)) = Map.insert key val myMap

tappend :: TS a -> TS a -> TS a
tappend (TS [] []) ts2 = ts2
tappend ts1 (TS [] []) = ts1
tappend (TS k1 v1) (TS k2 v2) =
    let
        bothKeys = mconcat [k1,k2]
        completeKeys = [minimum bothKeys..maximum bothKeys]
        kvMap = foldl insertMaybePair Map.empty (zip k1 v1)
        updatedMap = foldl insertMaybePair kvMap (zip k2 v2)
        combinedValues = map (\v -> Map.lookup v updatedMap) completeKeys
    in
        TS completeKeys combinedValues

instance Semigroup (TS a) where
    (<>) = tappend

instance Monoid (TS a) where
    mempty = TS [] []

ts1 = fileToTs file1
ts2 = fileToTs file2
ts3 = fileToTs file3
ts4 = fileToTs file4

tsAll = mconcat [ts1,ts2,ts3,ts4]

mean :: (Real a) => [a] -> Double
mean xs = total/count
    where
        total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTs :: (Real a) => TS a -> Maybe Double
meanTs (TS _ []) = Nothing
meanTs (TS ks vs)
    | all (== Nothing) vs = Nothing
    | otherwise =
        let
            justVals = filter isJust vs
            cleanVals = map fromJust justVals
            avg = mean cleanVals
        in
            Just avg

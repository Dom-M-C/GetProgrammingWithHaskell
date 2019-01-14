module Chapter19 where

import qualified Data.Map as M
import Chapter18
import Data.Maybe

possibleDraws :: [Int]
possibleDraws = [1..50]


getDraws :: [Int] -> M.Map Int Organ -> [Maybe Organ]
getDraws ids catalog = map getContents ids
    where getContents = \x -> M.lookup x catalog


availableOrgs :: [Maybe Organ]
availableOrgs = getDraws possibleDraws organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length justOrgans
    where justOrgans = filter (\x -> Just organ == x) available

isSomething Nothing = False
isSomething (Just _) = True

justOrgans = filter isSomething availableOrgs

showOrgan (Just organ) = show organ
showOrgan Nothing = ""

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat org) = show org <> " in a vat"
    show (Cooler org) = show org <> " in a cooler"
    show (Bag org) = show org <> " in a bag"

instance Semigroup Int where
    (<>) = (+)

instance Monoid Int where
    mempty = 0

maybeInt :: Maybe Int -> Int
maybeInt Nothing = mempty
maybeInt (Just x) = x

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ


placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)


process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) =
    show container <>
    " in the " <>
    show location

processRequest :: Int -> M.Map Int Organ -> String
processRequest id catalog = processAndReport organ
    where
        organ = M.lookup id catalog
        processAndReport (Just x) = report (process x)
        processAndReport Nothing = error "key not found"

emptyDraws = length $ filter isNothing availableOrgs

maybeMap f = map maybeLambda
    where
        maybeLambda (Just x) = f x
        maybeLambda Nothing = Nothing

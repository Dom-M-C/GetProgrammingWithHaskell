module Chapter29 where

import Chapter28
import Data.List

appMult = (*) <$> Just 2 <*> Just 7

cartAdd :: Num a => [a] -> [a] -> [a]
cartAdd l1 l2 = pure (+) <*> l1 <*> l2

cartProd :: [a] -> [b] -> [(a, b)]
cartProd l1 l2= pure (,) <*> l1 <*> l2

doorPrize :: [Int]
doorPrize = [1000,2000,5000]

boxPrize :: [Int]
boxPrize = [500,25000]

allPrizes = pure (+) <*> doorPrize <*> boxPrize

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where
        twoThroughN = [2..n]
        composite = pure (*) <*> twoThroughN <*> twoThroughN
        isNotComposite = not . (`elem` composite)


testNames :: [String]
testNames =
    [   "John Smith"
    ,   "Robert' DROP TABLE Students;--"
    ,   "Christina NULL"
    ,   "Randall Munroe"
    ,   "Dominic Copeland"
    ]

testIds :: [Int]
testIds =
    [   1337
    ,   0123
    ,   999999
    ,   42
    ]

testScores :: [Int]
testScores =
    [   0
    ,   1000000
    ,   -999999
    ,   666
    ]

testUsers :: [User]
testUsers = pure User <*> testNames <*> testIds <*> testScores

allFMap :: Applicative f => (a -> b) -> f a -> f b
allFMap f = (<*>) $ pure f

exampleMaybe :: Maybe Int
exampleMaybe = pure exp
    where exp = (*) ((+) 2 4) 6


beerPacks = [6,12]

beersDrunk = 4

friendsComing = [2,3]

avgDrinks = [3,4]

beerTarget = pure (*) <*> friendsComing <*> avgDrinks

beersLeft :: [Integer]
beersLeft = map (\x -> x - 4) beerPacks
--beersLeft = abs . (-4) <$> beerPacks

neededBeers = pure (-) <*> beerTarget <*> beersLeft

--avg :: Num a => [Int] -> a
avg xs = (sum xs) / (fromIntegral $ length xs)

--shouldBuy = target = avg $ map fromIntegral neededBeers





module Chapter21 where

import System.Random
import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "hello " <> name

putMain :: IO ()
putMain = do
    putStrLn "Hi, tell me your name!"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

newtype Die = Die Int deriving Show

instance Bounded Die where
    minBound = (Die 1)
    maxBound = (Die 6)

rollMain :: IO ()
rollMain = do
    dieRoll <- randomRIO (1, 6) :: IO Int
    putStrLn (show dieRoll)

areaFromDiameter :: Size -> Size
areaFromDiameter size = pi * (size/2) ^ 2

type Size = Double
type Cost = Double

type Pizza = (Size, Cost)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaFromDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2
    | cost1 > cost2 = p2
    | otherwise = p1
    where
        cost1 = costPerInch p1
        cost2 = costPerInch p2

describePizza :: Pizza -> String
describePizza p@(size, cost) = "the pizza of size " <> show size
    <> " and cost " <> show cost
    <> " has cost per inch " <> show (costPerInch p)

mainPizza :: IO ()
mainPizza = do
    putStrLn "what is the size of pizza 1"
    size1 <- getLine
    putStrLn "what is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "what is the size of pizza 2"
    size2 <- getLine
    putStrLn "what is the cost of pizza 2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)


mainMaybe :: Ord k => k -> Map.Map k String -> Maybe String
mainMaybe n m = do
    name <- Map.lookup n m
    let maybeName = helloPerson name
    return maybeName


data Alcohol = Alcohol { volume :: Double, price :: Double, percent :: Double }

calculateAlcoholPerPrice (Alcohol vol pri per) = (vol * per) / pri

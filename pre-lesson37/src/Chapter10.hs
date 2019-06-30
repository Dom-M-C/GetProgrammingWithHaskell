module Chapter10 where

type FluidOunces = Integer

data Cup = Coffee FluidOunces deriving(Show)

--cup :: FluidOunces -> (FluidOunces -> Cup) -> Cup
cup flOz = \message -> message flOz

aCup = cup 6

xCup x = cup x

getOz c = c (\f -> f)

drink c oz = cup (floz - oz)
    where floz = getOz c


robot (name, attack, hp) = \constructor -> constructor (name, attack, hp)

killerRobot = robot ("Kill3r", 25, 300)

name (n, _, _) = n
dmg (_, d, _) = d
hp (_, _, hp) = hp

getName aRobot = aRobot name

setName aRobot newName = aRobot (\(n,a,h) -> robot (newName, a, h))

printRobot aRobot = aRobot(\(n,a,h) -> n ++ ", attack: " ++ (show a) ++ ", hit points: " ++ (show h))

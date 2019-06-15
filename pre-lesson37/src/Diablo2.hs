module Diablo2 where


criticalStrike = 25.0
deadlyStrike = 25.0

doubleDmgChance cs ds = let
        deadlyStrikeChance = (ds / 100) * (100 - cs)
    in
        cs + deadlyStrikeChance

between :: Ord a => a -> a -> a -> Bool
between low high a = a >= low && a <= high

openWoundsDmgPerFrame clvl = let
    lvlDmg
        | between 1  15 clvl = 9  * clvl + 31
        | between 16 30 clvl = 18 * clvl - 104
        | between 31 45 clvl = 27 * clvl - 374
        | between 46 60 clvl = 36 * clvl - 779
        | between 61 99 clvl = 45 * clvl - 1319
        | otherwise = error "level not a valid character level"
    in
        lvlDmg / 256

openWoundsDmg frames clvl =
    openWoundsDmgPerFrame clvl * frames

playerOpenWoundsDmg :: Double -> Double -> Double
playerOpenWoundsDmg a b = openWounds / 4.0
    where openWounds = openWoundsDmg a b

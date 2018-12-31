module Diablo2 where


criticalStrike = 25.0
deadlyStrike = 25.0

doubleDmgChance cs ds = let
        deadlyStrikeChance = (ds / 100) * (100 - cs)
    in
        cs + deadlyStrikeChance

openWoundsDmgPerFrame clvl = let
    lvlDmg
        | clvl > 0  && clvl <= 15 = 9  * clvl + 31
        | clvl > 15 && clvl <= 30 = 18 * clvl - 104
        | clvl > 30 && clvl <= 45 = 27 * clvl - 374
        | clvl > 45 && clvl <= 60 = 36 * clvl - 779
        | clvl > 60 && clvl <= 99 = 45 * clvl - 1319
    in
        lvlDmg / 256

openWoundsDmg frames clvl =
    openWoundsDmgPerFrame clvl * frames

playerOpenWoundsDmg :: Int -> Int -> Double
playerOpenWoundsDmg a b = fromIntegral (openWounds a b) / 4.0
    where openWounds = undefined--(\x y -> openWoundsDmg x y)

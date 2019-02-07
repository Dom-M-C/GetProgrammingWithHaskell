module Chapter27 where

import qualified Data.Map as Map

successRequest = Just 1

funcInc = (+1) <$> successRequest

funcShow = show <$> successRequest

{-
instance Functor Maybe where
    fmap f (Just n) = Just (f n)
    fmap _ Nothing = Nothing
-}

funcRev = reverse <$> (Just "something")


data RobotPart = RobotPart
    {   name :: String
    ,   description :: String
    ,   cost :: Double
    ,   count :: Int
    } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
    {   name = "left arm"
    ,   description = "left arm for wine drinking"
    ,   cost = 1000.00
    ,   count = 3 }

rightArm :: RobotPart
rightArm = RobotPart
    {   name = "right arm"
    ,   description = "left arm for hand gestures"
    ,   cost = 1025.00
    ,   count = 5 }

robotHead :: RobotPart
robotHead = RobotPart
    {   name = "rowboat head"
    ,   description = "this head was made for thinking"
    ,   cost = 5092.25
    ,   count = 2 }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat
    [   "<h2>", partName, "</h2>"
    ,   "<p><h3>desc</h3>", partDesc, "</p>"
    ,   "<p><h3>cost</h3>", partCost, "</p>"
    ,   "<p><h3>count</h3>", partCount, "</p>"]
    where
        partName  = name part
        partDesc  = description part
        partCost  = show (cost part)
        partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where
        keys = [1..3]
        vals = [leftArm,rightArm,robotHead]
        keyVals = zip keys vals

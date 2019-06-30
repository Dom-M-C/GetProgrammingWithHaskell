module Chapter27 where

import qualified Data.Map as Map
import Data.Maybe

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

partsDb :: Map.Map Int RobotPart
partsDb = Map.fromList keyVals
    where
        keys = [1..3]
        vals = [leftArm,rightArm,robotHead]
        keyVals = zip keys vals

--insertSnippet :: Maybe Html -> IO ()
--insertSnippet

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDb

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDb

htmlPartsDb = renderHtml <$> partsDb

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

data Relationship = Killer | Friendly deriving Show

data Creature a = Robot a | Wolf a | Human a deriving Show

instance Functor Creature where
    fmap f (Robot r) = Robot (f r)
    fmap f (Wolf w) = Wolf (f w)
    fmap f (Human h) = Human (f h)

newtype Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box b) = Box (f b)

moreBoxes :: Int -> Box a -> Box [a]
moreBoxes n b = replicate n <$> b

newtype Vec a = Vec [a]
newtype Mat a = Mat [Vec a]

putInBox :: Box a -> Box (Box a)
putInBox b = Box <$> b

unwrapBox :: Box (Box a) -> Box a
unwrapBox (Box (b)) = b

lookupMain :: IO ()
lookupMain = do
    putStrLn "enter an ID"
    inp <- readLn
    let outp = Map.lookup inp partsDb
    putStrLn (fromJust $ show <$> outp)

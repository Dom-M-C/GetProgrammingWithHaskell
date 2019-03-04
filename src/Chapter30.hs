module Chapter30 where

import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

names =
    [   "nYarlathoTep"
    ,   "KINGinYELL0W"
    ,   "dagon1997"
    ,   "rcarter1919"
    ,   "xCTHULUx"
    ,   "yogSOThoth"
    ]

userNameDb :: Map.Map GamerId UserName
userNameDb = Map.fromList $ zip [1..6] names

creditsDb :: Map.Map UserName PlayerCredits
creditsDb = Map.fromList $ zip names [2000,15000,300,12,50000,150000]

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId gid = lookupUserName gid >>= lookupCredits

lookupUserName :: GamerId -> Maybe UserName
lookupUserName gid = Map.lookup gid userNameDb

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits uname = Map.lookup uname creditsDb

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just uname) = lookupCredits uname

type WillCoId = Int

gamerIdDb :: Map.Map WillCoId GamerId
gamerIdDb = Map.fromList $ zip [1001..1006] [1..6]

coidToGamerId :: WillCoId -> Maybe GamerId
coidToGamerId coid = (Map.lookup coid gamerIdDb)

coidToCredits :: WillCoId -> Maybe PlayerCredits
coidToCredits coid = coidToGamerId coid
    >>= lookupUserName
    >>= lookupCredits


echo :: IO ()
echo =  putStrLn "enter a string to be echoed"
    >> getLine
    >>= putStrLn
    >> putStrLn "ta"

askForName :: IO ()
askForName = putStrLn "tell me your name"

nameReply name = "Hello, " <> name <> "!"

helloName = (askForName >> getLine)
    >>= (\x -> return (nameReply x))
    >>= putStrLn

allFMapM :: Monad m => (a -> b) -> m a -> m b
allFMapM f x = x >>= (\y -> return (f y))

--allApp :: Monad m => m (a -> b) -> m a -> m b
allApp f x = return (\y ->  y >>= f) <$> x

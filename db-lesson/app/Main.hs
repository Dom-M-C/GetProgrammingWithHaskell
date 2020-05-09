module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

main :: IO ()
main = do
    print "Enter command: "
    command <- getLine
    performCommand command



data Tool = Tool
    {   toolId :: Int
    ,   name :: String
    ,   description :: String
    ,   lastReturned :: Day
    ,   timesBorrowed :: Int
    ,   quantityOwned :: Int
    }

data User = User
    {   userId :: Int
    ,   userName :: String
    }

instance Show User where
    show user = mconcat
        [   show $ userId user
        ,   ".) "
        ,   userName user
        ]

instance Show Tool where
    show tool = mconcat
        [   show $ toolId tool, ".) "
        ,   name tool
        ,   "\n\t- description: "   , description   tool
        ,   "\n\t- last returned: "  , show $ lastReturned  tool
        ,   "\n\t- times borrowed: ", show $ timesBorrowed tool
        ,   "\n\t- quantity owned: ", show $ quantityOwned tool
        ,   "\n"
        ]


addUser :: String -> IO ()
addUser userName = do
    conn <- open "tools.db"
    execute
        conn
        "INSERT INTO user (username) VALUES (?)"
        (Only userName)
    print $ mconcat ["user added: ", userName]
    close conn

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn

withToolsDb = withConn "tools.db"

addUser' userName =
    withToolsDb $ \conn -> do
        execute conn "INSERT INTO user (username) VALUES (?)"
            (Only userName)
        print $ mconcat ["user added: ", userName]

checkout :: Int -> Int -> IO ()
checkout userId toolId = withToolsDb $
    \conn -> do
        execute conn
            insertStatement
            (userId, toolId)
    where
        insertStatement = mconcat
            [   "INSERT INTO Checkedout (UserId, ToolId, QuantityCheckedout) VALUES (?, ?, 1)"
            ,   "   ON CONFLICT(UserId, ToolId)"
            ,   "   DO UPDATE SET QuantityCheckedout = QuantityCheckedout + 1;"
            ]


instance FromRow User where
    fromRow = User
        <$> field
        <*> field

instance FromRow Tool where
    fromRow = Tool
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field

printUsers :: IO ()
printUsers = withToolsDb $
    \conn -> do
        resp <- query_ conn "SELECT * FROM User;" :: IO [User]
        mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withToolsDb $
    \conn -> do
        resp <- query_ conn q :: IO [Tool]
        mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM Tool"


printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat
    [   "SELECT * "
    ,   "FROM Tool "
    ,   "WHERE NOT EXISTS"
    ,   "(  SELECT *"
    ,   "   FROM Checkedout"
    ,   "   WHERE ToolId = Id"
    ,   "   AND QuantityOwned <= QuantityCheckedout"
    ,   ");"
    ]

printCheckedout :: IO ()
printCheckedout = printToolQuery $ mconcat
    [   "SELECT * "
    ,   "FROM Tool "
    ,   "WHERE EXISTS"
    ,   "(  SELECT * "
    ,   "   FROM Checkedout"
    ,   "   WHERE ToolId = Id"
    ,   "   AND QuantityCheckedout > 0"
    ,   ")"
    ]

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x


selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn
        "SELECT * FROM tool where id = (?)"
        (Only toolId) :: IO [Tool]
    return $ firstOrNothing resp


updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
    {   lastReturned = date
    ,   timesBorrowed = 1 + timesBorrowed tool
    }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "tool id not found"
updateOrWarn (Just tool) = withToolsDb $
    \conn -> do
        let q = mconcat
                [   "UPDATE Tool"
                ,   "   SET lastReturned = ?"
                ,   "   ,  timesBorrowed = ?"
                ,   "   WHERE id = ?;"
                ]
        execute conn q
            (   lastReturned tool
            ,   timesBorrowed tool
            ,   toolId tool
            )
        print "tool updated"


updateToolTable :: Int -> IO ()
updateToolTable toolId = withToolsDb $
    \conn -> do
        tool <- selectTool conn toolId
        currentDay <- utctDay <$> getCurrentTime
        let updatedTool = updateTool
                <$> tool
                <*> pure currentDay
        updateOrWarn updatedTool

checkin :: Int -> Int -> IO ()
checkin userId toolId = withToolsDb $
    \conn -> do
        let updateStmt = mconcat
                [   "UPDATE Checkedout "
                ,   "   SET QuantityCheckedout = QuantityCheckedout - 1"
                ,   "   WHERE QuantityCheckedout > 0"
                ,   "   AND userId = ?"
                ,   "   AND toolId = ?;"
                ]
        execute conn updateStmt (userId, toolId)

checkinAndUpdate :: Int -> Int -> IO ()
checkinAndUpdate userId toolId = do
    checkin userId toolId
    updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
    print "Enter new user name"
    userName <- getLine
    addUser' userName

promptAndCheckout :: IO ()
promptAndCheckout = do
    print "User id?"
    userId <- pure read <*> getLine
    print "Tool id?"
    toolId <- pure read <*> getLine
    checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
    print "User id?"
    userId <- pure read <*> getLine
    print "Tool id?"
    toolId <- pure read <*> getLine
    checkinAndUpdate userId toolId

performCommand :: String -> IO ()
performCommand command
    | command == "users" = printUsers >> main
    | command == "tools" = printTools >> main
    | command == "adduser" = promptAndAddUser >> main
    | command == "checkout" = promptAndCheckout >> main
    | command == "checkin" = promptAndCheckin >> main
    | command == "in" = printAvailable >> main
    | command == "out" = printCheckedout >> main
    | command == "quit" = print "bye!"
    | otherwise = print "command not found" >> main

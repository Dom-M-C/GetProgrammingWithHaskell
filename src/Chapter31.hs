module Chapter31 where

import Chapter21
import qualified Data.Map as Map


askForName :: IO ()
askForName = putStrLn "tell me your name"

nameReply name = "Hello, " <> name <> "!"

helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    print (nameReply name)

helloNameNoSugar :: IO ()
helloNameNoSugar = askForName
    >> getLine
    >>= (\name -> return $ nameReply name)
    >>= putStrLn

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
    {   candidateId :: Int
    ,   codeReview :: Grade
    ,   cultureFit :: Grade
    ,   education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where
        passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [   passedCoding
                ,   passedCultureFit
                ,   educationMin
                ]

testCan = Candidate 0 C C PhD
failedCan = viable testCan

readId :: IO Int
readId = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate = Candidate
    <$> (putStrLn "Id?" >> readId)
    <*> (putStrLn "Code grade?" >> readGrade)
    <*> (putStrLn "Culture grade?" >> readGrade)
    <*> (putStrLn "Degree?" >> readDegree)

assesCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    return (if passed then "pass" else "nope")

oneCan = Candidate 0 A C PhD
twoCan = Candidate 1 A B MS
threeCan = Candidate 2 A A BA
fourCan = Candidate 3 A B HS
fiveCan = Candidate 4 B C MS

candidateList = [oneCan,twoCan,threeCan,fourCan,fiveCan]

candidateDb = Map.fromList . map (\x -> (candidateId x, x))
    $ candidateList

assesCandidateMaybe :: Int -> Maybe String
assesCandidateMaybe cId = do
    candidate <- Map.lookup cId candidateDb
    let passed = viable candidate
    return (if passed then "pass" else "nope")

assesCandidateList :: [Candidate] -> [String]
assesCandidateList candidates = do
    candidate <- candidates
    let passed = viable candidate
    return (if passed then "pass" else "nope")

assesCandidate :: Monad m => m Candidate -> m String
assesCandidate candidates = do
    candidate <- candidates
    let passed = viable candidate
    return (if passed then "pass" else "nope")

(|>) = flip ($)
(.>) = flip (.)

--rewrite of Chapter21.mainPizza
mainPizzaSansDo :: IO ()
mainPizzaSansDo =  putStrLn "what is the size of pizza 1" >> getLine
    >>= (\size1 -> putStrLn "what is the cost of pizza 1" >> getLine
    >>= (\cost1 -> putStrLn "what is the size of pizza 2" >> getLine
    >>= (\size2 -> putStrLn "what is the cost of pizza 2" >> getLine
    >>= (\cost2 -> return (read size1 :: Double, read cost1 :: Double)
    >>= (\pizza1 -> return (read size2 :: Double, read cost2 :: Double)
    >>= (\pizza2 -> return (comparePizzas pizza1 pizza2)
    >>= (\betterPizza -> putStrLn (describePizza betterPizza)
    >> return ()    ))))))) --lambda festival


mainPizzaSansDoAltFormat = putStrLn "what is the size of pizza 1" >>
    getLine >>= (\size1 -> putStrLn "what is the cost of pizza 1" >>
    getLine >>= (\cost1 -> putStrLn "what is the size of pizza 2" >>
    getLine >>= (\size2 -> putStrLn "what is the cost of pizza 2" >>
    getLine >>= (\cost2 ->
    return (read size1, cost1) >>= (\pizza1 ->
    return (read size2, cost2) >>= (\pizza2 ->
    return (comparePizzas pizza1 pizza2) >>= (\betterPizza ->
    putStrLn (describePizza betterPizza) >>
    return ()   )))))))

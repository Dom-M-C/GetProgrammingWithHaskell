module Chapter17 where


import Data.Semigroup


--QC 17.2

instance Semigroup Int where
    (<>) a b = a `div` b

data Color = Red
    | Yellow
    | Blue
    | Green
    | Purple
    | Orange
    | Brown
    | White deriving (Show, Eq)

instance Semigroup Color where
    (<>) White x = x
    (<>) x White = x
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Blue Yellow = Green
    (<>) Yellow Blue  = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b
        | a == b = a
        | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
        | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
        | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
        | otherwise = Brown

--Semigroup has to be associative

--type class laws can require certain behaviour
{-
instance Monoid Color where --breaks the first law of monoids - mappend mempty x = x
    mempty = Brown
-}

--Q17.1
instance Monoid Color where
    mempty = White

--17.3.3

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs =
    let
        totalProbs = sum probs
        normalizedProbs = map (\x -> x / totalProbs) probs
    in
        PTable events normalizedProbs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "\t|\t", show prob, " \n"]

instance Show PTable where
    show (PTable a b) = mconcat $ zipWith showPair a b

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 =
    let
        nToAdd = length l2
        repeatedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2
    in
        zipWith func newL1 cycledL2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 =
    let
        combiner = \x y -> mconcat [x, "-", y]
    in
        cartCombine combiner e1 e2

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
    (<>) ptab (PTable [] []) = ptab
    (<>) (PTable [] []) ptab = ptab
    (PTable e1 p1) <> (PTable e2 p2) =
        let
            newEvents = combineEvents e1 e2
            newProbs = combineProbs p1 p2
        in
            createPTable newEvents newProbs

instance Monoid PTable where
    mempty = PTable [] []


coin :: PTable
coin = createPTable ["heads", "tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

data Coin = Heads | Tails
data Prob = Prob Double

--instance Semigroup Coin where
--    ()


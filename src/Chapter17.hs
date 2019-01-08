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
    | Brown deriving (Show, Eq)

instance Semigroup Color where
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

instance Monoid Color where --breaks the first law of monoids - mappend mempty x = x
    mempty = Brown

--17.3.3

type Events = [String]
type Probs = [Double]





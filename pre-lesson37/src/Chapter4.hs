module Chapter4 where

import Data.List

names :: [FullName]
names = [("Dominic", "Copeland")
    , ("Scarlet", "Broker")
    , ("Daniel", "Copeland")
    , ("Sally", "Maidment")
    , ("Amelia", "Copeland")]

sortLast name1 name2 =
    if lastName1 > lastName2
        then GT
    else if lastName1 < lastName2
        then LT
    else EQ
    where
        lastName1 = snd name1
        lastName2 = snd name2

sortFirst name1 name2 =
    if firstName1 > firstName2
        then GT
    else if firstName1 < firstName2
        then LT
    else EQ
    where
        firstName1 = fst name1
        firstName2 = fst name2

type FullName = (String, String)
type Name = String

sortTuple :: (FullName -> Name) -> FullName -> FullName -> Ordering
sortTuple func fullName1 fullName2 =
    if name1 > name2
        then GT
    else if name1 < name2
        then LT
    else EQ
    where
        name1 = func fullName1
        name2 = func fullName2

sortFirst' = compareTuple fst
sortLast' = compareTuple snd

sortFirstThenLast a b =
    if sortFirst' a b == EQ
        then sortLast' a b
    else
        sortFirst' a b

sortLastThenFirst a b =
    if sortLast' a b == EQ
        then sortFirst' a b
    else
        sortLast' a b

compareLast name1 name2 =
    compare lastName1 lastName2
    where
        lastName1 = snd name1
        lastName2 = snd name2

compareTuple :: (FullName -> Name) -> FullName -> FullName -> Ordering
compareTuple func fullName1 fullName2 =
    compare name1 name2
    where
        name1 = func fullName1
        name2 = func fullName2

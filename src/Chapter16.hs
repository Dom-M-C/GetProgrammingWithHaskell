module Chapter16 where

import Chapter14 --For Name

--Quick Check 16.2
data MyCar = CarWithSpoiler | SansSpoilerCar

--answer
data Spoiler = Spoiler | NoSpoiler
data Car = Car
data SportsCar = SportsCar Car Spoiler

--Listing 16.10


data Creator = AuthorCreator Author | ArtistCreator Artist
data Author = Author Name
data Artist = Person Name | Band String

hpLovecraft = AuthorCreator
    (Author
        (TwoInitialsWithLast 'H' 'P' "Lovecraft"))


--16.16
type Price = Double

data Book = Book
    { author :: Creator
    , isbn :: String
    , bookTitle :: String
    , bookYear :: Int
    , bookPrice :: Price }

data VinylRecord = VinylRecord
    { artist :: Creator
    , recordTitle :: String
    , recordYear :: Int
    , recordPrice :: Price }

data CollectibleToy = CollectibleToy
    { toyName :: String
    , toyDescription :: String
    , toyPrice :: Price }

data Pamphlet = Pamphlet
    {   pamphletPrice :: Price
    }

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

price :: StoreItem -> Price
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pam) = 0

--QC 16.3
{-
madeBy :: StoreItem -> String
madeBy (BookItem book) = show $ author book
madeBy (RecordItem record) = show $ artist record
madeBy _ = error "toys aren't made by anyone"
-}

data Shape = Circle { radius :: Double }
    | Square { side :: Double }
    | Rectangle { depth :: Double, width :: Double }

area :: Shape -> Double
area (Circle r) = pi * (r ^2)
area (Square s) = s^2
area (Rectangle d w) = d * w

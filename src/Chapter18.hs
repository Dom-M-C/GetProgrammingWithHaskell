module Chapter18 where


import qualified Data.Map as Map


data Box a = Box a deriving Show



wrap a = Box a

unwrap (Box x) = x

boxmap f (Box x:xs) = Box(f x) : boxmap f xs

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

aPoint = Triple 0.1 53.2 12.3

tripleMap f (t:ts) = transform f t : tripleMap f ts

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

data List a = Empty | Cons a (List a) deriving Show

ourList :: List Int
ourList = Cons 1 (Cons 2 (Cons 3 Empty))


ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (Cons a rest) = Cons (f a) $ ourMap f rest


data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)


organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

orgMap = Map.fromList $ zip ids organs

{-
organInventory :: [Organ] -> Map.Map Organ Int
organInventory [] = mempty
organInventory (o:os) =

lookip = organ $ Map.lookup o
organ (Just x) = x
organ
-}


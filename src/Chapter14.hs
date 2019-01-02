module Chapter14 where

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving(Ord)

instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

instance Eq SixSidedDie where
    (==) S1 S1 = True
    (==) S2 S2 = True
    (==) S3 S3 = True
    (==) S4 S4 = True
    (==) S5 S5 = True
    (==) S6 S6 = True
    (==) _ _ = False

instance Enum SixSidedDie where
    toEnum 1 = S2
    toEnum 2 = S3
    toEnum 3 = S4
    toEnum 4 = S5
    toEnum i
        | i == 0 || i > 5 = S1
        | i == 5 || i < 0 = S6

    fromEnum S1 = 0
    fromEnum S2 = 1
    fromEnum S3 = 2
    fromEnum S4 = 3
    fromEnum S5 = 4
    fromEnum S6 = 5

type First = String
type Middle = String
type Last = String

data Name = BiName (First, Last) | TriName (First, Middle, Last) deriving (Show, Eq)

instance Ord Name where
    compare (BiName(f1, l1)) (BiName(f2, l2)) = compare (l1, f1) (l2, f2)
    compare (BiName(f1, l1)) (TriName(f2, _, l2)) = compare (l1, f1) (l2, f2)
    compare (TriName(f1, _, l1)) (BiName(f2, l2)) = compare (l1, f1) (l2, f2)
    compare (TriName(f1, m1, l1)) (TriName(f2, m2, l2)) = compare (l1, f1, m1) (l2, f2, m2)

names =
     [BiName("Dominic", "Copeland")
    , TriName("Scarlet", "Holly", "Broker")
    , BiName("Daniel", "Copeland")
    , BiName("Sally", "Maidment")
    , BiName("Amelia", "Copeland")]

data EnumForEasyOrd = First | Second | Third | Fifth deriving(Enum, Show)

instance Ord EnumForEasyOrd where
    (<=) x y = fromEnum x <= fromEnum y

instance Eq EnumForEasyOrd where
    (==) x y = fromEnum x == fromEnum y

class Eq a => Die a where
    roll :: a

instance Die EnumForEasyOrd where
    roll = First


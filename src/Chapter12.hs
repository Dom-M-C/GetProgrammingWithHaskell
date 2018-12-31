module Chapter12 where


data Tep = D Int Double

getDouble :: Tep -> Double
getDouble = \(D _ y) -> y

data Sex = Female | Male

instance Show Sex where
    show Male = "Bloke"
    show Female = "Lady"

data BloodGroup = A | B | AB | O deriving(Show)
data RhType = Pos | Neg

data BloodType = BloodType BloodGroup RhType

instance Show BloodType where
    show (BloodType grp rh) = show grp ++ case rh of
        Pos -> "+"
        Neg -> "-"

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True --universal donor
canDonateTo _ (BloodType AB _) = True --universal recipient
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

dom = BloodType O Neg
scarlet = BloodType A Neg

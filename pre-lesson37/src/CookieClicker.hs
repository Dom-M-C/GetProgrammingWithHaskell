module CookieClicker where



bankNeeded cps = round $ (900*cps) / 0.15

bankNeededFrenzy = (*7) . bankNeeded


data Object = Building
    {   cost :: Double
    ,   cps :: Double
    ,   name :: String
    }

objectValue ob = cps ob / cost ob

compareObjects ob1 ob2 = compare (objectValue ob1) (objectValue ob2)

portal = Building 0 0 "Portal"


instance Show Object where
    show x = name x
        <> ", costs: " <> (show $ cost x)
        <> ", produces: " <> (show $ cps x)
        <> ", value ratio: " <> (show $ objectValue x)

module Chapter15 where

class LowerCaseChar a where
    toLower :: a -> a
    toLowerString :: [a] -> [a]

instance LowerCaseChar Char where
    toLower c
        | fromEnum c < 48 = ' '
        | fromEnum c < 58 = c
        | fromEnum c < 97 = toEnum ((fromEnum c) + 32) :: Char
        | fromEnum c > 122 = 'z'
        | otherwise = c
    toLowerString str = map toLower str

{-
instance Bounded CipherAlphabet where
    minBound = LowerCaseChar 'a'
    maxBound = LowerCaseChar 'z'
-}
class LowerCaseChar a => Cipher a where
    encrypt :: a -> a
    decrypt :: a -> a


instance Cipher Char where
    encrypt c
        | c == ' ' = ' '
        | c `elem` (map (toEnum) [48..57] :: [Char]) = c

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving(Show,Enum,Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where
        halfAlphabet = alphabetSize `div` 2
        offset = fromEnum c + halfAlphabet
        rotation = offset `mod` alphabetSize

rotChar :: Char -> Char
rotChar charToRot = rotN sizeOfAlphabet charToRot
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

rotString :: String -> String
rotString str = map rotChar str




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
class LowerCaseChar a => CipherZ a where
    encrypt :: a -> a
    decrypt :: a -> a


instance CipherZ Char where
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

----


xorBool :: Bool -> Bool -> Bool
xorBool a b = (a || b) && (not (a && b))

xorPair :: (Bool, Bool) -> Bool
xorPair (b1,b2) = xorBool b1 b2

xor :: [Bool] -> [Bool] -> [Bool]
xor ls1 ls2 = map xorPair (zip ls1 ls2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
    let
        remainder = n `mod` 2
        nextVal = n `div` 2
    in
        if remainder == 0
        then False : intToBits' nextVal
        else True : intToBits' nextVal

--15.16

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n =
    let
        reversedBits = reverse $ intToBits' n
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])
    in
        leadingFalses ++ reversedBits

charToBits :: Char -> Bits
charToBits c = intToBits $ fromEnum c

--bitsToInt :: Bits -> Int
bitsToInt bits@(b:bs) =
    let
        size = length bits
        indices = [size-1, size-2 ..0]
        trueLocations = filter (\x -> fst x == True) $ zip bits indices
    in
        sum $ map (\x -> 2 ^ snd x) trueLocations

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "salty"

myPlainText :: String
myPlainText = "hello cryptography"

stringToBits :: String -> [Bits]
stringToBits = map charToBits

bitsToString :: [Bits] -> String
bitsToString = map bitsToChar

applyPad' :: String -> String -> [Bits]
applyPad' pad text =
    let
        paddedPad = take (length text) $ cycle pad
        padAsBits = stringToBits paddedPad
        textAsBits = stringToBits text
        zippedBits = zip padAsBits textAsBits
    in
        map (\pair -> fst pair `xor` snd pair ) zippedBits

applyPad :: String -> String -> String
applyPad pad text = bitsToString $ applyPad' pad text

encodeDecodeWithMyPad :: String -> String
encodeDecodeWithMyPad = applyPad myPad

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot
data OneTimePad = Pad String

instance Cipher Rot where
    encode Rot text = rotString text
    decode Rot text = rotString text

instance Cipher OneTimePad where
    encode (Pad pad) text = applyPad pad text
    decode (Pad pad) text = applyPad pad text

myOneTimePad :: OneTimePad
myOneTimePad = Pad (cycle [minBound .. maxBound])

--pseudo random number generator: PRNG
--linear congruential generator algorithm
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNo seed = (a * seed + b)  `mod` maxNo

examplePrng :: Int -> Int
examplePrng = prng 1337 7 100

prngPad :: Int -> [Int]
prngPad seed = examplePrng seed : (prngPad (prng (seed - 7) (seed + 7) (seed * 7) seed))

myStreamPad = Pad (bitsToString $ map intToBits $ prngPad 5)

data StreamCipher = Stream OneTimePad

--todo: SteamCipher implementation
{-
instance Cipher StreamCipher where
    encode (Stream (Pad )) text = applyPad pad text
    decode (Stream myStreamPad pad) text = applyPad pad text
-}

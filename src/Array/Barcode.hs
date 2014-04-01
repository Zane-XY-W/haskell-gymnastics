{-#LANGUAGE ScopedTypeVariables#-}
module Barcode where
import Data.Function
import qualified Data.Map  as M
import Data.Char
import Data.Array
import Data.Ix
import Data.Ratio
import Control.Applicative
import Data.Word8
import Data.List

-- | apply the function to every other element of the list
-- this is a typical usage of zipWith <$>
-- id :: a -> a
-- cycle :: [a -> a] -> [a -> a]
-- cycle [f,id] :: [a -> a]
-- ($) :: (a -> a) -> a -> a
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- it may not be obvious that,the function omited a [a] type at both side
-- of the definition
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

-- | the last digit of the barcodes
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum ds' `mod` 10) -- ^ evaluated from left to right
  where ds' = mapEveryOther (*3) (reverse ds)

-- | foldA for array isn't defined in stardard lib because there're
-- multiple ways to fold two dimensional arrays, here is a fold for one
-- dimentional array
foldA :: (Ix k) => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = rec s (indices a)
  where rec seed (x : xs)
          = let s' = f seed (a ! x) in s' `seq` rec s' xs
        rec seed _ = seed

foldA1 :: (Ix k) => (a -> a ->a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a

leftOddList, leftEvenList, rightList, parityList :: [String]

leftOddList
  = ["0001101", "0011001", "0010011", "0111101", "0100011",
     "0110001", "0101111", "0111011", "0110111", "0001011"]

-- | rightList is the complement of the left code table
rightList = map complement <$> leftOddList -- ^ map f lift the f to take a list (here is [Char]),
  where complement '0' = '1'
        complement '1' = '0'

leftEvenList = map reverse rightList

parityList
  = ["111111", "110100", "110010", "110001", "101100", "100110",
     "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs

-- | convert list to Array encoding code tables
leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String
leftOddCodes  = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes    = listToArray rightList
parityCodes   = listToArray parityList

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

-- | takes a length of 12 list, the check digit is calculate during the
-- process
encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) = outerGuard : leftEncodings ++ centerGuard : rightEncodings ++ [outerGuard]
  where parityRow = parityCodes ! first
        (left, right) = splitAt 5 rest
        leftEncodings = zipWith encodeLeft parityRow left -- ^ leftEncodings is enocded with parityRow
        rightEncodings =  map encodeRight (right ++ [checkDigit s])


encodeLeft :: Char -> Int -> String
encodeLeft '1' = (leftOddCodes !)
encodeLeft '0' = (leftEvenCodes !)

encodeRight :: Int -> String
encodeRight = (rightCodes !)

outerGuard, centerGuard :: String
outerGuard  = "101"
centerGuard = "01010"

type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)
type Pixmap = Array (Int, Int) RGB

luminance :: RGB -> Pixel
luminance (r,g,b) = round (r' * (0.30::Double) + g' * 0.59 + b' * 0.11)
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

type Greymap = Array (Int,Int) Pixel

pixmapToGreymap :: Pixmap -> Greymap
pixmapToGreymap = fmap luminance

data Bit = Zero | One deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
  where binary i | i < pivot = Zero
                 | otherwise = One
        pivot = round $ least + (greatest - least) * n
        least = fromIntegral $ choose (<) a
        greatest = fromIntegral $ choose (>) a
        choose f = foldA1 $ \x y -> if f x y then x else y

type Run = Int
type RunLength a = [(Run, a)]

-- |
-- let bits = [0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0]
-- runLength bits
-- [(2,0),(2,1),(2,0),(2,1),(6,0),(4,1),(4,0)]
runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group
  where rle xs = (length xs , head xs)
-- | get only the length
-- runLengths bits
-- [2,2,2,2,6,4,4]
runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength

type Score = Ratio Int

-- |
-- Data.Ratio (%) :: Integral a => a -> a -> Ratio a
scaleToOne :: [Run] -> [Score]
scaleToOne xs = map (% sum xs) xs

type ScoreTable = [[Score]]

scaleLen :: [String] -> ScoreTable
scaleLen = map (scaleToOne . runLengths)

scaledLeftOdd, scaledLeftEven, scaledRight, scaledParity :: ScoreTable
scaledLeftOdd = scaleLen leftOddList
scaledLeftEven = scaleLen leftEvenList
scaledRight = scaleLen rightList
scaledParity = scaleLen parityList

-- | Given two scaled run length sequences, we can calculate an approximate “distance” between them
-- ghci > let g = scaleToOne [2,6,4,4]
-- ghci > [1 % 8,3 % 8,1 % 4,1 % 4]
-- ghci > distance g (head scaledLeftEven)
-- ghci > 13 % 28
-- An exact match will give a distance of zero, with weaker matches resulting in larger distances
distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

type Digit = Word8

-- | Given a scaled run length table, we choose the top 3 matches in that table for a given input sequence
bestScores :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScores scoreTable ps = take 3 . sort $ scores
  where scores = zip [distance scoreList (scaleToOne ps) | scoreList <- scoreTable] [0..9]

-- | For each match in the left group, we have to remember whether we found it in the even parity table or the odd table
data Parity a = Even a | Odd a | None a deriving (Show)

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
    fmap = parityMap

-- compareWithoutParity :: (Ord a) => Parity a -> Parity b -> Ordering
compareWithoutParity = compare `on` fromParity

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity
  (map Odd (bestScores scaledLeftOdd ps) ++
  map Even (bestScores scaledLeftEven ps))

bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores scaledRight

chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h,t) = f xs
                 in h : chunkWith f t

chunkOf :: Int -> [a] -> [[a]]
chunkOf n = chunkWith (splitAt n)

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_,One):_) = []
candidateDigits rle | length rle < 59 = []
                    | any null match = []
                    | otherwise  = map (map (fmap snd)) match
  where match = map bestLeft left ++ map bestRight right
        left = chunkOf 4 . take 24 . drop 3 $ runLengths
        right = chunkOf 4 . take 24 . drop 32 $ runLengths
        runLengths = map fst rle

type Map a = M.Map Digit [a]

type DigitMap = Map Digit
type ParityMap = Map (Parity Digit)

insertMap :: Digit -> Digit -> [a] -> Map a -> Map a
insertMap key digit v m = v `seq` M.insert key' v m
  where key' = (key + digit) `mod` 10

updateMap :: Parity Digit -- ^ new digit
          -> Digit -- ^ existing key
          -> [Parity Digit] -- ^ existing digit seqenence
          -> ParityMap -- ^ map to update
          -> ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit = new `M.union` M.foldWithKey (updateMap digit) M.empty old



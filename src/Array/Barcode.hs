module Barcode where
import Data.Array
import Data.Ix

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

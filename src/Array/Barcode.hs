module Barcode where

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

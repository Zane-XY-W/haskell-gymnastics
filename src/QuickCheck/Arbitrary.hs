module Arbitrary where
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck

data Ternary = Yes | No | Unknown deriving (Eq,Show)

instance Arbitrary Ternary where
    arbitrary = elements [Yes, No, Unknown]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r
  where l = filter (< x) xs
        r = filter (>= x) xs

-- | use ==> to filter out invalid null list before running the tests
-- prop_minimum :: forall a. Ord a => [a] -> Property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

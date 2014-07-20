module Recursion where


sumRange :: ( Num a, Ord a ) => a -> a -> a -> a
sumRange start end step = sumRange' start 0
  where sumRange' s rs =
          if s <= end
             then sumRange' (s + step) (rs + s)
             else rs

module Spaghetii where

fac :: Int -> Int
fac x = 
  let 
    rec n acc 
      | n > 1 = rec (n - 1) (acc * n)
      | otherwise =  acc
  in rec x 1


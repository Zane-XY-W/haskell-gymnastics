{-# LANGUAGE ScopedTypeVariables #-}
module RecursionSpec where

import           Recursion
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "sumRange" $ do
  describe "sumRange1" $ do
    it "should be the same as sum list" $ property $ do
     sumRange 1 7 2 == sum [1,3..7]
  describe "sumRange2" $ do
    it "should be the same as sum list function" $ property $
      \(x::Int) (y::Int) -> (x < y) ==> sumRange x y 1 == sum [x..y]

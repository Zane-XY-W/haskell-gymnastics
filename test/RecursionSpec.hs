{-# LANGUAGE ScopedTypeVariables #-}
module RecursionSpec where

import           Recursion
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Property

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "sumRange" $
    it "should be the same as sum list" $
      sumRange 1 7 2 `shouldBe` sum [1,3..7]
    -- it "should b the same as sum list function" $
    --   property $ \x y -> sumRange x y 1 == sum [x..y]

module IO.ReadAndWriteSpec (main, spec) where

import           Data.List
import           IO.ReadAndWrite
import           System.IO
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Prac1" $ do
  describe "readTxt" $ do
    it "should print something" $ do
      readTxt
  describe "sumRange" $ do
    it "should sum range" $ do
      sumRange 1 3 1 `shouldBe` 6
  describe "filterLines" $ do
    it " should return a list of TODOs" $ do
      handle <- openFile "README.md" ReadMode
      size <- fmap length $ filterLines handle (\x -> "TODO" `isPrefixOf` x)
      size `shouldBe` 4
      hClose handle
  describe "filterLines3" $ do
    it " should return a list of TODOs" $ do
      size <- fmap length $ filterLines3  "README.md" (\x -> "TODO" `isPrefixOf` x)
      size `shouldBe` 6
  describe "writeList" $ do
    it "should write content to new file" $ do
      handle <- openFile "new.txt" WriteMode
      writeList handle ["a","b","c"]
      hClose handle
    it "should contains 3 lines after previous write" $ do
      handle <- openFile "new.txt" ReadMode
      c <- countLines handle
      print c
      hClose handle

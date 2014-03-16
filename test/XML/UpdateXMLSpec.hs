{-# LANGUAGE OverloadedStrings #-}
module XML.UpdateXMLSpec where

import Test.Hspec
-- import Test.HUnit hiding (Test)
import qualified Data.ByteString.Lazy.Char8 as LC
import XML.UpdateXML

main :: IO ()
main = hspec spec

spec :: Spec
spec= describe "updating xml in place " $
   it "should update the xml in place" updateInplaceTest'

xmlFrag :: LC.ByteString
xmlFrag = LC.concat ["<a>", "...", "<d>", "<e>", "<f>some data to change</f>", "</e>", "</d>", "...", "</a>"]

-- updateInplaceTest :: IO ()
-- updateInplaceTest = putStrLn $ LC.unpack $ updateNodeInplace xmlFrag

updateInplaceTest' :: IO ()
updateInplaceTest' = putStrLn $ LC.unpack $ updateNodeInplace' xmlFrag



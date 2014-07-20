{-# LANGUAGE OverloadedStrings #-}
module FourPicsOneWord where
import           Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import           Data.Text.Encoding
import           Network.HTTP.Conduit  (simpleHttp)
import           Text.HTML.DOM         (parseLBS)
import           Text.XML.Cursor

dictSource :: String -> Maybe String
dictSource n@(h : _)
  | h `elem` "5678" = Just $ concat ["http://www.poslarchive.com/math/scrabble/lists/common-", n, ".html"]
  | h `elem` "234"  = Just "http://www.poslarchive.com/math/scrabble/lists/common-234.html"
  | otherwise       = Nothing

extractDict :: Maybe String -> IO [BC.ByteString]
extractDict (Just url)
  = do cont <- simpleHttp url
       let wordsCursor = head $ fromDocument (parseLBS cont) $// element "pre" >=> child
           wordsBS     = encodeUtf8 . T.concat . content $ wordsCursor
       return $ concat $ BC.words <$> BC.lines wordsBS
extractDict _ = return []

main :: IO ()
main
  = do putStrLn "Enter the word length (5 - 8 digit): "
       n <- getLine
       putStrLn "Enter the 12 letters used to guess: "
       letters <- getLine
       filter (BC.all (`elem` letters)) <$> extractDict (dictSource n) >>= print

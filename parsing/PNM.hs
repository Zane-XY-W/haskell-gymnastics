module PNM where
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

-- sample data
-- P3
-- # The same image with width 3 and height 2,
-- # using 0 or 1 per color (red, green, blue)
-- 3 2 1
-- 1 0 0   0 1 0   0 0 1
-- 1 1 0   1 1 1   0 0 0
data Greymap = Greymap {
             greyWidth :: Int,
             greyHeight :: Int,
             greyMax :: Int,
             greyData :: L.ByteString
             } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ " " ++ show h ++ " " ++ show m

matchHeader :: C.ByteString -> C.ByteString -> Maybe C.ByteString
matchHeader prefix str
  | prefix `L.isPrefixOf` str
    = Just (C.dropWhile isSpace (C.drop (C.length prefix) str))
  | otherwise = Nothing

{-
- Generate possible word from "xdfajrboekfv"
- -}
module WordsFromLetters where
import Data.List
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Applicative
import Collection.General
import Criterion.Measurement

source :: String
source = sort "xdfajrboekfv"

consonants :: String
consonants = "xdfjbkfv"

hasTwoVowels :: String -> Bool
hasTwoVowels x = any (not . null . (`intersect` x)) ["ae", "ao", "eo"]

-- remove a,e,o,r from original input
nonWordSeq :: [String]
nonWordSeq = concatMap permutations (combinations 2 consonants)

impossibleWord :: String -> Bool
impossibleWord x = any (`isInfixOf` x) nonWordSeq

possibleCombs :: [String]
possibleCombs = filter hasTwoVowels $ combinations 5 source

possiblePerms :: [LC.ByteString]
possiblePerms = map LC.pack $ filter (not . impossibleWord) (concatMap permutations possibleCombs)

loadDict :: IO [LC.ByteString]
loadDict = do c <- LC.readFile "./test/resources/5letterwords.txt"
              return (concat $ LC.words <$> LC.lines c)

printDiff :: IO ()
printDiff = intersect possiblePerms <$> loadDict >>= print

hasAllLetters :: LC.ByteString -> Bool
hasAllLetters  = LC.all (`elem` source)

-- "9.541988 ms"
printFiltered :: IO ()
printFiltered =  filter hasAllLetters <$> loadDict >>= print

-- print with time benchmark
main :: IO ()
main = secs <$> time_ printFiltered >>= print


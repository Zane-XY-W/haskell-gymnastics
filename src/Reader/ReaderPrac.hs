{-# LANGUAGE TemplateHaskell #-}

module ReaderPrac where
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.All

type Config = Map String Int;

configs = Map.fromList [("count",3), ("1",1), ("b",2)]

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Config -> Bool
isCountCorrect = runReader checkCount

-- The Reader monad, which implements this complicated check.
checkCount :: Reader Config Bool
checkCount
  = do count <- asks (lookupVar "count")
       confMap <- ask
       return (count == (Map.size confMap))

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Config -> Int
lookupVar name bindings = fromJust (Map.lookup name bindings)

getConfigNums :: Reader String Int
getConfigNums
  = do content <- ask
       return (length content)

-- |
-- local: Executes a computation in a modified environment
calculateModifiedContentLen :: Reader String Int
calculateModifiedContentLen = local ("Prefix " ++) getConfigNums

-- The Reader/IO combined monad, where Reader stores a string.
printReaderContent :: ReaderT String IO ()
printReaderContent
  = do content <- ask
       liftIO $ putStrLn ("The Reader Content: " ++ content)

prop_isCountCorrect = isCountCorrect configs == True

prop_local
  = do let s = "12345"
       let modifiedLen = runReader calculateModifiedContentLen s
       let len = runReader getConfigNums s
       modifiedLen == len

main = $quickCheckAll

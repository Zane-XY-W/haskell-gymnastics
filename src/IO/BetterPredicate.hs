{-#LANGUAGE ScopedTypeVariables#-}
module BetterPredicate where
import System.IO(hFileSize, IOMode(..), withFile)
import System.FilePath()
import System.Directory(Permissions(..), getPermissions, getModificationTime)
import Control.Monad(filterM)
import Data.Time
import IO.RecursiveContents(getRecursiveContents)
import Control.Exception

type Predicate = FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check path' = do
          perm <- getPermissions path'
          size <- getFileSize path'
          time <- getModificationTime path'
          return (p path' perm size time)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_::SomeException) -> return Nothing) $
  withFile path ReadMode $ \h -> do
    size <- hFileSize h
    return (Just size)

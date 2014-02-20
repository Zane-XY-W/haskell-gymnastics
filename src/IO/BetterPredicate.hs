module BetterPredicate where

import System.IO(hClose, openFile, hFileSize, IOMode(..))
import System.FilePath()
import System.Directory(Permissions(..), getPermissions, getModificationTime)
import Control.Monad(filterM)
import System.Time(ClockTime(..))
import IO.RecursiveContents(getRecursiveContents)

type Predicate = FilePath -> Permissions -> Maybe Integer -> ClockTime -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check path' = do
          perm <- getPermissions path'
          size <- getFileSize path'
          time <- getModificationTime path'
          return (p path' perm size time)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing)::SomeException -> IO Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return Just size

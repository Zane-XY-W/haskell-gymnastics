{-# LANGUAGE ScopedTypeVariables #-}
module IO.ControledVisit where
import           Control.Exception
import           Control.Monad
import           Data.Time
import           System.Directory  (Permissions (..), getDirectoryContents,
                                    getModificationTime, getPermissions)
import           System.FilePath
import           System.IO         (IOMode (ReadMode), hFileSize, withFile)

data Info = Info{infoPath    :: FilePath,
                 infoPerms   :: Maybe Permissions,
                 infoSize    :: Maybe Integer,
                 infoModTime :: Maybe UTCTime}
          deriving (Eq, Ord, Show)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path
  = do names    <- getUsefulContents path
       contents <- mapM getInfo (path : map (path </>) names)
       liftM concat $
         forM (order contents) $
           \ info ->
             if isDirectory info && infoPath info /= path then
               traverse order (infoPath info) else return [info]

getUsefulContents :: FilePath -> IO [FilePath]
getUsefulContents path
  = do names <- getDirectoryContents path
       return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act
  = handle (\ (_ :: SomeException) -> return Nothing)
      (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path
  = do perms                                 <- maybeIO (getPermissions path)
       size                                  <- maybeIO (withFile path ReadMode hFileSize)
       modTime                               <- maybeIO (getModificationTime path)
       return (Info path perms size modTime)

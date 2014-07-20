module FindFiles where
import           Control.Monad.Writer
import           Data.List
import           Data.Sequence
import           System.Directory     (doesDirectoryExist, getCurrentDirectory,
                                       getDirectoryContents)
import           System.FilePath      ((</>))

main :: IO ()
main =  do dir <- getCurrentDirectory
           collectFiles dir ".hs" >>= print
           collectFiles dir ".md" >>= print

-- | takes a FilePath and extension
findFiles :: FilePath -> String -> WriterT (Seq FilePath) IO ()
findFiles path ext =  do
    fileNames' <- liftIO (getDirectoryContents path)
    let fileNames = Data.List.filter (not . ("." `isPrefixOf`)) fileNames'
    forM_ fileNames $ \fileName -> do
      when (fileName /= "." && fileName /= ".." && ext `isSuffixOf` fileName ) $ tell (singleton fileName)
      let fullName = path </> fileName
      isDir <- liftIO (doesDirectoryExist fullName)
      when isDir $ findFiles fullName ext

collectFiles :: FilePath -> String -> IO (Seq FilePath)
collectFiles path ext = execWriterT  (findFiles path ext)

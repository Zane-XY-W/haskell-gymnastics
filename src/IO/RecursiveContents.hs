module RecursiveContents(getRecursiveContents) where
import System.Directory(doesDirectoryExist, getDirectoryContents)
import System.FilePath((</>))
import Control.Monad(forM)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  files <- getDirectoryContents topDir
  let normalFiles = filter (`notElem` [".", ".."]) files
  files' <- forM normalFiles $ \file -> do
     let path = topDir </> file
     exists <- doesDirectoryExist path
     if exists
       then getRecursiveContents path
       else return [path]
  return (concat files')

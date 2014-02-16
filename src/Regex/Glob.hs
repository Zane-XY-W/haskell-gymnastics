module Regex.Glob (namesMatching) where
import System.Directory(doesFileExist,doesDirectoryExist,getCurrentDirectory,getCurrentContent)
import System.FilePath(dropTrailingPathSeperator, splitFileName, (</>))
import Control.Exception
import Control.Monad
import Regex.GlobRegex (matchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "*?[]")

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    exists <- doesFileExist name
    if exists
      then return True
      else doesDirectoryExist name

isHidden ('.':_) = True
isHidden _ = False

--listMatches function returns a list of all files matching the given glob
--pattern in a directory

listMatches :: FilePath -> String -> IO [String]
listMatches dir pat = do
  dir' <- if null dir
           then getCurrentDirectory
           else return dir
  handle (const (return [])) $ do
    names <- getDirectoryContents dir'
    let names' =
          if isHidden pat
            then filter isHidden names
            else filter (not . isHidden) names
    return (filter  (`matchesGlob` pat) names')

-- check if a baseName exists under a dirName
listPlain :: FilePath -> String -> IO String
listPlain dirName baseName = do
    exists <- if null baseName
                then doesDirectoryExist dirName
                else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])

-- takes a single string contains path patterns
namesMatching :: String -> IO [String]
namesMatching pat
  | not (isPattern pat) = do
    if doesNameExist pat
      then return [pat]
      else return []
  | case splitFileName pat of
      ("", baseName) -> do
        curDir <- getCurrentDirectory
        listMatches curDir baseName
      (baseDir, baseName) -> do
        dirs <- if isPattern baseDir
                  then namesMatching (dropTrailingPathSeperater baseDir)
                  else [baseDir]
        let listFiles = if isPattern baseName
                            then listMatches
                            else listPlain
        files <- forM dirs $ \dir -> do
          files' <- listFiles dir baseName
          return (map (dir </>) files')
        return (concat files)

module ReadConfig where

import Control.Exception
import System.IO
import Control.Monad
import System.Directory
import System.FilePath
-- | read .config.txt from ~/ print error if not found
readConfig :: IO String
readConfig
  = catch (liftM (</> ".gitconfig2") getHomeDirectory >>= readFile)
      (\ e ->
         do let err = show (e :: IOException)
            hPutStr stderr err
            return err)

-- catch more than one exceptions using catches
--

readDivisor :: IO Int
readDivisor = do
  putStrLn "enter the divisor:"
  divisor <- getLine
  return (100 `div` read divisor)


main :: IO ()
main = print =<< readDivisor

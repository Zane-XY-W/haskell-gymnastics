module ForkIO where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, handle)
import qualified Data.ByteString.Lazy as L
import System.Console.Readline(readline)
import Codec.Compression.GZip (compress)

compressFile :: IO ()
compressFile
  = do maybeLine <- readline "Enter filename with full path: "
       case maybeLine of
           Nothing -> return ()
           Just name | (not . null) name ->
                       handle (print :: (IOException -> IO ())) $
                         do content <- L.readFile name
                            forkIO (compressFile name content)
                            return ()
                     | otherwise -> return ()
  where compressFile path = L.writeFile (path ++ ".gz") . compress

module IO.ReadAndWrite where

import           Control.Applicative
import           System.IO

readTxt :: IO()
readTxt =  readFile "README.md" >>= putStrLn



filterLines :: Handle -> (String -> Bool) -> IO [String]
filterLines h p  = filterLines' []
  where filterLines' :: [String] -> IO [String]
        filterLines' rs = do
          eof <- hIsEOF h
          if eof
            then return rs
            else do
              line <- hGetLine h
              filterLines' (if p line then rs ++ [line] else rs)

filterLines2 :: Handle -> (String -> Bool) -> IO [String]
filterLines2 h p = do
    content <- hGetContents h
    return ( filter p $ lines content )

filterLines3 :: FilePath -> (String -> Bool) -> IO [String]
filterLines3 path p = filter p . lines <$> readFile path

writeList :: Handle -> [String] -> IO ()
writeList handle = mapM_ (hPutStrLn handle)


countLines :: Handle -> IO Int
countLines handle = countRec 0
  where countRec :: Int -> IO Int
        countRec count = do
          eof <- hIsEOF handle
          if eof then return count else countRec (count + 1)

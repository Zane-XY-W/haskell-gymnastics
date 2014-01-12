module InputAndOutput where

import Text.Regex

{-
- for input and output, you ususally only need putStrLn and getLine
- -}

{-
- let stmt inside do 
- -}
readInput ::  IO ()
readInput = do 
  putStrLn "enter your input:"
  line <- getLine
  -- process input
  let split = splitRegex (mkRegex "\\s+") 
  mapM_ putStrLn $ split line

{-
- if-then-else stmt inside do
- -}
guessMe :: Int -> IO ()
guessMe num = do
    putStrLn "enter your guess:"
    n <- getLine
    if read n > num
      then do 
	      putStrLn "too big"
	      guessMe num
      else if read n < num
	then do
	  putStrLn "too small"
	  guessMe num
	else putStrLn "win"
	  

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module TestException where

import Control.Exception
import Data.Typeable.Internal(Typeable)
import Text.Regex

data MyException = ThisException | ThatException
  deriving (Show, Typeable)

instance Exception MyException

testCatch ::  IO ()
testCatch = throw ThisException `catch` \e -> putStrLn ("Caught " ++ show (e :: MyException))

catchMultiple :: IO Int
catchMultiple = (do 
  putStrLn "enter x and y seperated by space:"
  line <- getLine
  let x : y : _  = splitRegex  ( mkRegex "\\s+" ) line
  return (( read x :: Int ) `div` ( read y :: Int )))  `catches`
  [Handler ( \(_ :: ArithException)-> return 0),
   Handler ( \(_ :: IOException) -> return 0)
  ]

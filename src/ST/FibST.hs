module FibST where

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef

-- | return the N-th Fibonacci number using constant space
-- all read, write, modify operations in Data.STRef, takes STRef returns ST
-- STRef is the direct handle you'll be working with ST monad, so STRef
-- should be the equivanlent accumulator in imperative algorithms.
-- a type check, but wrong implementation:
-- @
-- fibSTWrong :: Integer -> Integer
-- fibSTWrong n
--   = if n < 2 then 1 else
--       let xST = newSTRef 1
--           yST = newSTRef 1
--         in runST $ fibST' n xST yST >>= readSTRef
--   where fibST' 1 xST _ = xST
--         fibST' n' xST yST
--           = do xRef <- xST
--                x <- readSTRef xRef
--                yRef <- yST
--                y <- readSTRef yRef
--                writeSTRef xRef y
--                writeSTRef yRef $! x + y
--                fibST' (n' - 1) xST yST -- ^STRef is mutable, but ST is not
-- @
fibST :: Integer -> Integer
fibST n
  = if n < 2 then n else
      runST $ -- ^ runST takes a ST s a returns a
        do xRef <- newSTRef 1
           yRef <- newSTRef 1
           fibST' n xRef yRef
  where fibST' 1 xRef _ = readSTRef xRef -- ^xRef will contains the N-th STRef at last
        fibST' n' xRef yRef
          = do x <- readSTRef xRef
               y <- readSTRef yRef
               writeSTRef xRef y -- ^this is mutating xRef, writeSTRef returns ST s ()
               writeSTRef yRef $! x + y
               fibST' (n' - 1) xRef yRef -- ^xRef and yRef are both mutated in the writeSTRef

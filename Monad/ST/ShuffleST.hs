module Shuffle where

import Data.Array.ST
import Control.Monad.ST
import Data.STRef

import Control.Monad.State
import System.Random

-- | Randomly shuffle a list without the IO Monad O(N)
shuffle :: [a] -> StdGen -> ([a],StdGen)
shuffle xs gen = runST $ do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- arr n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen')
  where
    n = length xs
    arr :: Int -> [a] -> ST s (STArray s Int a)
    arr m =  newListArray (1,m)

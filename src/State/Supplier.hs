{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module State.Supplier (
              Supplier,
              next,
              runSupplier
              ) where

import Control.Monad.State

-- | embeds a list of states and a result a
newtype Supplier s a = S (State [s] a) deriving (Monad)

-- | updates Supplier internally
next :: Supplier s (Maybe s)
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> put xs >> return (Just x)

-- | feed input states into Supplier
runSupplier :: Supplier s a -> [s] -> (a, [s])
runSupplier (S m) = runState m

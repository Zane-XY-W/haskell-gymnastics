{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module State.SupplierM(
                   SupplierM(..),
                   S.Supplier, -- ^re-exporting Supplier classes, so the client can only import MonadSupplier
                   S.runSupplier
                   ) where
import qualified State.Supplier as S
import System.Random hiding (next)
import Control.Arrow

{-
- Separating Interface from Implementation
- -}

-- | embeds a s to monad m, s could be anything
-- | m -> s means `s` must appear along `m` in the same type
-- bypass a missing `a` in the function types.
class (Monad m) => SupplierM s m | m -> s where
  -- | next should update state s inside the monad internally, any
  -- instances should override this function
  next :: m (Maybe s)

-- | a Supplier monad embeds a s, Supplier is a State monad
instance SupplierM s (S.Supplier s) where
  -- | mutates State monad internally
  next = S.next -- ^ because of the functional dependencies, we could not mention s yet pass the type check

showTwo :: (Show s) => S.Supplier s String
showTwo = do
    a <- next
    b <- next
    return (show "a: " ++ show a ++ show "b: " ++ show b)

showTwoRandom :: (Show s, Random s) => S.Supplier s String
showTwoRandom = do
    r1 <- next
    r2 <- next
    return (show "r1: " ++ show r1 ++ show "r2: " ++ show r2)

randIO :: Random a => IO [a]
randIO = getStdRandom $ \g ->
  let (a, b) = split g
  in (randoms a, b)

randIO':: Random a => IO [a]
randIO' = getStdRandom (first randoms . split)

main :: IO ()
main = do
    print . fst $ S.runSupplier showTwo ["A", "B"]
    print . fst $ S.runSupplier showTwo ["A"]
    randInts <- randIO' :: IO [Integer]
    print . fst $ S.runSupplier showTwoRandom randInts

module ST where
import Control.Monad.ST
import Data.STRef
import Control.Monad

-- | sum using a ST
-- a value of type STRef s a is a mutable variable in state thread s, containing a value of type a
sumST :: Num a => [a] -> a
sumST xs = runST $ do
  n <- newSTRef 0 -- ^newSTRef :: a -> ST s (STRef s a)
  forM_ xs $ \x -> modifySTRef n (+x) -- ^modifySTRef :: STRef s a -> (a -> a) -> ST s ()
  readSTRef n -- ^readSTRef :: STRef s a -> ST s a


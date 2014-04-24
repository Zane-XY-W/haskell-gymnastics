module MonadPlusUsage where
import Control.Monad
import Data.Maybe

-- | find the first Just a in a list of Maybe a
firstJust :: [Maybe a] -> Maybe a
firstJust = msum

zeroMod :: Int -> Int -> Maybe Int
x `zeroMod` n = guard ((x `mod` n) == 0) >> return x

-- if [] contains n Just a, return Just n, else return Nothing
containsN:: MonadPlus m => Int -> [Maybe a] -> m Int
containsN n xs = guard ((length . take n) (filter isJust xs) == n) >> return n



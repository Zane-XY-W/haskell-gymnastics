module Char where
import Data.Char

-- | create decimal Int from binary String
-- 1011101 = (1 * 2^0) + (0 * 2^1) + (1 * 2^3) + (1 * 2^4) + (0 * 2^5) + (1 * 2^6) = 1 + 4 + 8 + 16 + 64 = 93
fromBinary :: String -> Int
fromBinary str = sum $ zipWith toDec (reverse str) [0::Int .. ]
  where toDec a b = digitToInt a * (2 ^ b)

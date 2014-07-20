module Parse where

import qualified Data.ByteString.Lazy as LB
import           Data.Int

data ParseState = ParseState {
                string :: LB.ByteString,
                offset :: Int64
                }

data Parse a = Parse {
                runParse :: ParseState -> Either String (a, ParseState)
                }


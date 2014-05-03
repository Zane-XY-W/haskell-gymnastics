module CollectLogs where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Text.Lazy
import           Data.Text.Lazy.Builder
import           Prelude                hiding (lines)

main :: IO ()
main = print $ collectLogs (pack "SEVERE error 1 found \n INFO \n  SEVERE error 2 found \n")

collectLogs :: Text -> Text
collectLogs input
  = toLazyText $ execWriter $ do
      let filterMarker = pack "SEVERE"
      forM (lines input) $
        \ line -> when (filterMarker `isInfixOf` line) $ (tell . fromLazyText) line

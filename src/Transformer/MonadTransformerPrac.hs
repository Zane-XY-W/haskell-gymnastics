module MonadTransformerPrac where

import           Control.Monad.Reader
import           Control.Monad.State
import           System.Directory     (doesDirectoryExist, getDirectoryContents)
import           System.FilePath      ((</>))

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

-- stacking Monad Transformer
data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        st = AppState 0
    in runStateT (runReaderT k config) st

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path -- function application has the hightest precedence
  cfg <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO . doesDirectoryExist $ newPath
            if isDir && curDepth < cfgMaxDepth cfg -- cfgMaxDepth is field label, takes a data type defines it, returns the field
              then do -- then/else should indent under if
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $ put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest

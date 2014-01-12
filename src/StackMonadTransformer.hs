 module StackMonadTransformer where

import Control.Monad.Reader
import Control.Monad.State
import System.Directory
import System.FilePath
 
data AppConfig = AppConfig {
  cfgMaxDepth :: Int 
}

data AppState = AppState {
  stDeepestReached :: Int
}

type App = ReaderT AppConfig (StateT AppState IO) 

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]  
constrainedCount curDepth path = do
  contents <- liftIO . getDirectoryContents $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO . doesDirectoryExist $ newPath
    if ( isDir && curDepth < cfgMaxDepth cfg)
      then do 
        let newDepth = curDepth + 1
        st <- get
        when ( stDeepestReached st < newDepth) $ put st { stDeepestReached = newDepth }
        constrainedCount newDepth newPath
      else return []
  return $ (path, length contents) : concat rest
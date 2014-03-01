module IO.FoldDir where
import           IO.ControledVisit
import           System.FilePath

-- Continue will do the dir check
-- Skip will just goes to the next item

data Iterate seed = Done{unwrap :: seed}
                  | Skip{unwrap :: seed}
                  | Continue{unwrap :: seed}
                  deriving Show

-- function alias for function
-- takes iterate state
-- Info for a path
-- returns state wraps a seed type
type Iterator a = a -> Info -> Iterate a

-- takes a iterator function,
-- a seed for the iterator
-- a path to start with
-- returns result same as the seed's type

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree itr initSeed path
  = do endSeed <- fold initSeed path
       return (unwrap endSeed)
  where fold seed subpath = getUsefulContents subpath >>= walk seed
        walk seed (name : names)
          = do let path' = path </> name
               info <- getInfo path'
               case itr seed info of
                   done@(Done _) -> return done
                   Skip seed' -> walk seed' names
                   Continue seed' | isDirectory info ->
                                    do next <- fold seed' path'
                                       case next of
                                           done@(Done _) -> return done
                                           status -> walk (unwrap status) names
                                  | otherwise -> walk seed' names
        walk seed _ = return (Continue seed)

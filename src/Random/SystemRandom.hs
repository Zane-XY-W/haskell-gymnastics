module SystemRandom where

import Control.Monad.State.Strict
import System.Random

-- |
-- StdGen: is an instance of RandomGen, is a global random number generator.
type RandomState a = State StdGen a

-- |
-- using get and put to construct an stateful computation.
-- random :: (Random a, RandomGen g) => g -> (a, g)
getRandomGeneral ::  Random a => RandomState a
getRandomGeneral
  = get >>= \g -> let (v, g') = random g
                  in put g' >> return v

-- |
-- this function is doing the same thing as above, but more concise,
-- because the type of random :: (RandomGen g, Random a) => g -> (a, g)
-- is exactly matching a state function.
getRandom :: Random a => RandomState a
getRandom = state random

getTwoRandom :: Random a => RandomState (a, a)
getTwoRandom = liftM2 (,) getRandomGeneral getRandomGeneral

runTwoRandom :: IO (Int, Int)
runTwoRandom = do
    currentState <- getStdGen
    let (result, newState) = runState getTwoRandom currentState
    setStdGen newState
    return result

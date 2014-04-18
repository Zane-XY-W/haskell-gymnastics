module SystemRandom where

import Control.Monad.State.Strict
import System.Random

{-
- The class Random defineds a series random functions, like random and
- randomR, randomIO etc. For the type Random a, a is the random value type.
- The class RandomGen provides a common interface to random number generators.
-
- -}

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

-- |
-- getStdGen :: IO StdGen :  Gets the global random number generator.
-- setStdGen : Sets the global random number generator.
-- runState : unwrap a state monad, takes a State monad and initial state,
-- returns the result of the function and the new state.
runTwoRandom :: IO (Int, Int)
runTwoRandom = do
    currentState <- getStdGen
    let (result, newState) = runState getTwoRandom currentState
    setStdGen newState
    return result

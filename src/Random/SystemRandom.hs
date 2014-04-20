module SystemRandom where

import           Control.Monad.State.Strict
import           System.Random
-- import Control.Applicative

-- |
-- You can't modify the generator, need to reused the one returned from
-- previous call
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen
  = let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, _) = random newGen' -- ^ last gen is not useless
    in (firstCoin, secondCoin, thirdCoin)

type CoinS  = State StdGen

tossCoin :: CoinS Bool
tossCoin = do
    g <- get
    let (v, g') = random g
    put g'
    return v

-- |
-- call it with threeCoins' `fmap` newStdGen
threeCoins' :: StdGen -> (Bool, Bool, Bool)
threeCoins' = evalState $ liftM3 (,,) tossCoin tossCoin tossCoin

-- StdGen: is an instance of RandomGen, is a global random number generator.
type RandomState a = State StdGen a

-- |
-- using get and put to construct an stateful computation.
-- random :: (Random a, RandomGen g) => g -> (a, g)
getRandomGeneral :: (Random a) => RandomState a
getRandomGeneral
  = get >>= \ g -> let (v, g') = random g in put g' >> return v

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

data RandomInfo = RandomInfo {
                 rGen :: StdGen,
                 rNum :: Int
                }

type RState = State RandomInfo

getRandomNum :: Random a => RState a
getRandomNum = do
  rInfo <- get -- ^ get return RandomInfo, the monad is RState
  let (v, g') = random (rGen rInfo)
  put RandomInfo {rGen = g', rNum = rNum rInfo + 1}
  return v

getNum :: RState Int
getNum = rNum `liftM` get

-- | modify :: MonadState s m => (s -> s) -> m () : Maps an old state to
-- a new state inside a state monad. The old state is thrown away.
modifyNum :: Int -> RState ()
modifyNum a = modify (\st -> st {rNum = a })

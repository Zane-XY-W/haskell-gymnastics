module StateMonad where

import Control.Monad.State
import System.Random

{-
- Write a function print out the result of rolling a dice three times
- -}

type DiceState = State StdGen -- ^DiceState has State applied 1 type var, and is a Monad type

-- | rollDice returns a State monad
rollDice :: DiceState Int
rollDice = get >>=        -- ^get return m s, s is StdGen, so g is type of StdGen
    \g -> let (v, g') = randomR (1,6) g in put g' >> return v

rollDiceThreeTimes :: StdGen -> (Int, Int, Int)
rollDiceThreeTimes = evalState $ liftM3 (,,) rollDice rollDice rollDice

printDiceRoll :: IO ()
printDiceRoll = rollDiceThreeTimes `fmap` newStdGen >>= print


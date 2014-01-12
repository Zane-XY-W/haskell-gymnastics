module StateMonad where

import Control.Monad

newtype State s a = State{ runState:: s -> (a, s)}

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s 
                                      (State g) = f a
                                  in g newState 

type Stack = [Int]

pop :: State Stack Int
pop = State $ \(x : xs ) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a : xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  --a <- pop
  pop

-- conditional inside do
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8 
            
-- glue stateful computation
moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()              
  
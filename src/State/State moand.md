#The State Monad#

###how is it defined?###
The `mtl` package just reimported the `State` and `StateT` from `transformer` package, the actual source of `State` is defined in `transformer`, but `mtl` defines the `MonadState` class.


`type State s = StateT s Identity` 


or 

`type State s a = StateT s Identity a`

the `StateT s m` is defined as monad [ref](http://hackage.haskell.org/package/transformers-0.3.0.0/docs/src/Control-Monad-Trans-State-Strict.html#line-182) :

```
instance (Monad m) => Monad (StateT s m) where
    return a = state $ \s -> (a, s)
    m >>= k  = StateT $ \s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str
```
###the return and >>= function###
Notice the return function leaves the state unchanged, while >>= uses the final state of the first computation as the initial state of the second.

###the runState function###

`runState :: State s a -> s -> (a, s)` : unwrap a state monad, takes a State monad and initial state, returns the result of the function and the new state, it's the inverse of the `state` function.


#MonadState#

###how is it defined?###

This class is defined in `mtl`, a pretty general class for supporting `get` `put`. The definition is recursive, and notice the 'Minimal definition' comment at the top, means the instance of this class should provide implementation of either "get and put" or "state", because they can be implemented using each other. And in the `transformer` package, it only implements "get and put" [ref](http://hackage.haskell.org/package/transformers-0.3.0.0/docs/src/Control-Monad-Trans-State-Strict.html#line-204).

```
-- | Minimal definition is either both of @get@ and @put@ or just @state@
class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a
```
###get and put and modify###

- `get` copies `s` to `a`, leaves `s` unchanged
- `put s` copies new `s` to old `s` and set `a` to ()
- `return x` sets the `a` to x, leaves `s` unchanged
- because `get :: m s`, `m` is actually `State s`, a fully applied `m s` is like `StateT StdGen Identity StdGen`, so this is basically saying `get` copies `s` to `a`.
- you perform state operation using `put`, then `get` the computed state out
- before the computation completes, `a` is just a temp value, `put` sets `a` to `()` because the next `a` should be computed from the next `s`, not the previous `a`, and finally you copy `s` to `a` using `get`


```
runState (return []) 1 
-- so inside this monadic context, you know s is [] and a is Int. 
-- return sets a to [], runState sets s to 1, so the result is ([], 1)

```

```
runState get 1
-- inside this monadic context, s is set to 1, and get copy s to a
-- so the result is (1,1)
```

```
runState (put 0) 1
-- s is 1, and put set a to (), s to 0,
-- so the result is ((),0)
```

```
postincrement = do { x <- get; put (x+1); return x }
runState postincrement 1

-- get copied the original state out to x, put updated the state to (x + 1), 
-- but set the result to (), return set the result back to x, so it would be (s, (s + 1))
-- so the final result is (1,2)

```




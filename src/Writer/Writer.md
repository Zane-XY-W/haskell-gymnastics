#Writer

 
## how does it work
- `WriterT` is a monad over a `(result, output)` pair, sometimes the _result_ can also mean _input_, like in the `return v` context, just think it as a computed value.
- `Writer` is `WriterT` applied to `Identity` monad, so `Writer` just renames a `(result, output)` pair. 
- _result_ is the computed value, the _output_ is the collected monoid. 
- `exectWriter` returns the _output_ instead of the _result_.
- `tell` returns an empty `WriterT` with just single monoidal _output_ , the monoids get concatenated when `>>=` are performed.
-  `return a = writer (a, mempty)` return sets input value to a, and monoidal output to `mempty`
 
 
```
-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monoid w, Monad m) => w -> WriterT w m ()
tell w = writer ((), w) 
 
-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by the type @w@ of output to accumulate.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
type Writer w = WriterT w Identity

-- | Construct a writer computation from a (result, output) pair.
-- (The inverse of 'runWriter'.)
writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = writer (a, mempty)
    m >>= k  = WriterT $ do
        ~(a, w)  <- runWriterT m
        ~(b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
    fail msg = WriterT $ fail msg

 ```
 
## effectively append text with Builder and Writer

- `Data.Text.Lazy.Builder` is an instance of Monoid.
- `Writer Builder [()]` can be used to append Text

```
collectLogs :: Text -> Text
collectLogs input
  = toLazyText $ execWriter $ do
      let filterMarker = pack "SEVERE"
      forM (lines input) $
        \ line -> when (filterMarker `isInfixOf` line) $ (tell . fromLazyText) line



```

# MonadPlus

## laws
- `mzero` and `mplus` forms a `Monoid`
- `mzero` is a neutral element
    - `mzero mplus m  =  m`
    - `m mplus mzero  =  m`
- `mplus` is associative
- left distribution: a `mplus` b `>>=` f == (a `>>=` f ) `mplus` (b `>>=` f)

## relationships with `Monoid`

- `MonadPlus` and `Monoid` serve different purposes. [ref](http://stackoverflow.com/a/10168111/241824)
-  `Monoid` type is of kind *.
   ```
    class Monoid m where
        mempty :: m
        mappend :: m -> m -> m
    ```

- `MonadPlus` type is of kind * -> *
   ```
    class (Monad m) => MonadPlus m where
       mzero :: m a
       mplus :: m a -> m a -> m a
    ```
- m is a Monad, m a is a Monoid. [ref](http://stackoverflow.com/a/17056979/241824)

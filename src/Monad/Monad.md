#Monad#

###Monad and Functor###
----------
- In category theory, a `Monad` is a `Functor`, ideally, the `Monad` typeclass should be a subclass of `Functor`
- `Monad` type should be polymorphic to `Functor` type, now library authors have to define `Functor` `Applicative` instance for `Monad` instance.
- `join  :: (Monad m) => m (m a) -> m a` is defined using `>>=`: `join x=  x >>= id`
- you can define `>>=` using `join` and `fmap`:  `xs >>= f = join (fmap f xs)` 
- `join` is now defined in [Control.Monad][1], `Monad` typeclas is defined in [GHC.Base][2]. The AMP proposal will promote `join` into `Monad` typeclass. 

###[Applicative => Monad proposal (AMP)][3]###
----------
#### future-proofing current code ####
```
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
 
-- Monad m
 
instance Functor m where
    fmap = liftM
 
instance Applicative m where
    pure  = return
    (<*>) = ap
```

####duplicated functions####
- `pure` and `return` is the same
- `liftM` and `liftA` are `fmap` 
- `liftMn` are `liftAn`
- `<*>` is `ap`




  [1]: http://hackage.haskell.org/package/base-4.7.0.0/docs/src/Control-Monad.html#join
  [2]: http://hackage.haskell.org/package/base-4.7.0.0/docs/src/GHC-Base.html#Monad
  [3]: https://github.com/quchen/articles/blob/master/applicative_monad.md
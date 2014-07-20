#Monad#

## Monad and Functor
---
- In category theory, a `Monad` is a `Functor`, ideally, the `Monad` typeclass should be a subclass of `Functor`
- `Monad` type should be polymorphic to `Functor` type, now library authors have to define `Functor` `Applicative` instance for `Monad` instance.
- `join  :: (Monad m) => m (m a) -> m a` is defined using `>>=`: `join x=  x >>= id`
- you can define `>>=` using `join` and `fmap`:  `xs >>= f = join (fmap f xs)` 
- `join` is now defined in [Control.Monad][1], `Monad` typeclas is defined in [GHC.Base][2]. The AMP proposal will promote `join` into `Monad` typeclass. 

## Applicative => Monad [(AMP proposal)][3]
---
### future-proofing current code
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

### duplicated functions
- `pure` and `return` is the same
- `liftM` and `liftA` are `fmap` 
- `liftMn` are `liftAn`
- `<*>` is `ap`

## Monad Laws
---
### about left and right identity
Let (S, ∗) be a set S with a binary operation ∗ on it. Then an element e of S is called a left identity if e ∗ a = a for all a in S, and a right identity if a ∗ e = a for all a in S. If e is both a left identity and a right identity, then it is called a two-sided identity, or simply an identity. 
### laws

- **Left identity ( e ∗ a = a)**
	- return is a left identity for >>=
	- return >>= f  is the same as f
	- return x >>= f == f(x)
- **Right identity (a ∗ e = a)**
	- return is a right identity for >>=.
	- m >>= return still is m
	- m >>= return == m
- **Associativity**
	- The final monad law says that when we have a chain of monadic function applications with >>=, it shouldn't matter how they're nested 
	- Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)




  [1]: http://hackage.haskell.org/package/base-4.7.0.0/docs/src/Control-Monad.html#join
  [2]: http://hackage.haskell.org/package/base-4.7.0.0/docs/src/GHC-Base.html#Monad
  [3]: https://github.com/quchen/articles/blob/master/applicative_monad.md
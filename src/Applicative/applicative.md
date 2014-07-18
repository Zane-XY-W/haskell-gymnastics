applicative
======

#####lifeA and liftA2

```haskell
Prelude Control.Applicative> :t liftA
liftA :: Applicative f => (a -> b) -> f a -> f b

Prelude Control.Applicative> :t liftA2
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

Prelude Control.Applicative> liftA2 (:) (Just 3)(Just [4])
Just [3,4]
```

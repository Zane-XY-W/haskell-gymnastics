applicative
======

>Applicative functors are useful when you need sequencing of actions, but don't need to name any intermediate results. They are thus weaker than monads, but stronger than functors (they do not have an explicit bind operator, but they do allow running arbitrary functions inside the functor).
>
>When are they useful? A common example is parsing, where you need to run a number of actions that read parts of a data structure in order, then glue all the results together. This is like a general form of function composition:
>
>`f a b c d`
>
>where you can think of a, b and so on as the arbitrary actions to run, and f as the functor to apply to the result.
>
>`f <$> a <*> b <*> c <*> d`
>
>I like to think of them as overloaded 'whitespace'. Or, that regular Haskell functions are in the identity applicative functor.

#####lifeA and liftA2

```haskell
Prelude Control.Applicative> :t liftA
liftA :: Applicative f => (a -> b) -> f a -> f b

Prelude Control.Applicative> :t liftA2
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

Prelude Control.Applicative> liftA2 (:) (Just 3)(Just [4])
Just [3,4]
```

# QuickCheck


- a property is a function returns Bool, QC uses "prop_" to identify a property function.
- QC uses types to generate data, so you should give property function concrete types.
- use `(==>) :: Testable prop => Bool -> prop -> Property` filter data conditionally, notice that, it only take a single Bool, which means you cann't specifiy multiple isolated conditions.


## generate data

- `Gen a` is a generator for type `a`, you need a generator to generator any data.
- a `Gen a` is a monad.
- `arbitrary` defined in the `Arbitrary` typeclass, specified how a `Gen a` is produced.

### build-in arbitrary

```
sample (arbitrary ::Gen Int)
-1
2
...
```
```
sample (arbitrary ::Gen (Int, Int, Int))
(0,0,0)
(2,-2,1)
...
```

```
sample (arbitrary ::Gen (Positive Int, Positive Int, Positive Int))
(Positive {getPositive = 1},Positive {getPositive = 1},Positive {getPositive = 1})
(Positive {getPositive = 1},Positive {getPositive = 1},Positive {getPositive = 2})
...
```

### combinators

```
sample $ choose (1,9)

sample $ oneof [choose(1,3), choose (7,9)]

sample $ elements [98,99,100]

```
### [modifer](http://hackage.haskell.org/package/QuickCheck-2.7.3/docs/Test-QuickCheck-Modifiers.html) 

modifers applys to `a`
You can use newtype to create your own modifier.



### forAll 

forAll takes any `Gen a`

```
:t forAll
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
```
with arbitrary

```
verboseCheck $ forAll (arbitrary :: Gen (Positive Int)) $ \i -> i > 10
Failed:
Positive {getPositive = 1}
*** Failed! Falsifiable (after 1 test):
```

with combinators

```
verboseCheck $ forAll (choose (0, 9))  $ \i -> i > 0

```
forAll can also be nested

```
verboseCheck $ forAll (choose (0, 9))  $ \i -> 
				 forAll (choose (5,8)) $ \j -> i > j
```
# System.Random #



- The class `Random` defineds a series random functions, like `random` and `randomR`, `randomIO` etc. For the type Random a, a is the random value type.
- The class `RandomGen` provides a common interface to random number generators.
-  `StdGen`: is an instance of `RandomGen`, is a global random number generator.
-  `getStdGen :: IO StdGen` :  Gets the global random number generator, returns the same value if the global random generator is un-set.
-  `setStdGen` : Sets the global random number generator.
-  `newStdGen` : update global random generator at each call


```
> getStdGen
2146307971 1827928504
> getStdGen
2146307971 1827928504 --unchanged during consecutive calls
> newStdGen
204500098 1827928503 -- global random generator modified
> getStdGen
2146307972 1831677004 -- changed
```



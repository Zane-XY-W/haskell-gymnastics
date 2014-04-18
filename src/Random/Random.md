# System.Random #



- The class `Random` defineds a series random functions, like `random` and
`randomR`, `randomIO` etc. For the type Random a, a is the random value type.
- The class `RandomGen` provides a common interface to random number generators.
-  `StdGen`: is an instance of `RandomGen`, is a global random number generator.
-  `getStdGen :: IO StdGen` :  Gets the global random number generator.
- `setStdGen` : Sets the global random number generator.


#State Moand#

The state monad is often used with Random:

- `runState` : unwrap a state monad, takes a State monad and initial state,
returns the result of the function and the new state.
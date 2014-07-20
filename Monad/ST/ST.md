#ST#

###basics###

- `STRef s a` is a mutable variable in state thread `s`, containing a value of type `a`
- `STRef` is mutable, but not `ST`, your function would normally takes a `STRef` to perserve the state, and returns a `ST`
- all read, write, modify operations in `Data.STRef`, takes `STRef` returns `ST`
- `runST :: (forall s. ST s a) -> a`
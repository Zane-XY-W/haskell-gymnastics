Exception
======

## IO related exception

####Control.Exception
 - The SomeException type is the root of the exception type hierarchy.
 - if an expression throws multiple exceptions,
  1. the order maybe non-deterministic, because the evaluation is non-deterministic
  2. it will have multiple handlers, then use `catches :: IO a -> [Handler a] -> IO a`
 - the handler lambda need to be typed manually, like
    `\ (ex :: ArithException) -> handleArith ex`, otherwise the function body doesn't know what exception it is, so you may need to use language      pragma.

##### quick IO exception handling
```haskell
import Control.Exception.Base
handle (const (return []) :: IOError -> IO [String] ) $ do
  names <- getDirectoryContents dirName’
```
or change IOError to IOException, they’re the same.
note that, you don’t need to add pragma `{-# LANGUAGE ScopedTypeVariables #-}` to annotate an expression. [ref](http://www.haskell.org/onlinereport/exps.html#sect3.16)

old style handle function
```haskell
import Control.OldException (handle)
handle (const (return [])) $ do
  names <- getDirectoryContents dirName'
```
or, if you want to explicitly annotate the lambda type: `(\(_::SomeException) -> return Nothing)`,
you need to turn on `ScopedTypeVariables`
```haskell
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception (handle, SomeException)
handle (\(e::SomeException) -> (return [])) $ do ...
```

##### more readings
- [When do I use which function?](http://stackoverflow.com/a/6009807/1338198)
- [Control.Exception](https://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Exception.html)
- [Error vs Exception](http://www.haskell.org/haskellwiki/Error_vs._Exception)

## Non-IO exceptions
- [write your own ExceptT](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md)
- [Control.Monad.Trans.Except](http://hackage.haskell.org/package/transformers-0.4.1.0/docs/Control-Monad-Trans-Except.html#t:ExceptT)
- [8 ways to report errors in Haskell revisited](http://blog.ezyang.com/2011/08/8-ways-to-report-errors-in-haskell-revisited/)


### read, reads, maybeRead
- Exception is not pure, use `reads` if possible. [ref](http://stackoverflow.com/a/5121537/241824)
- read may throw `no parse` error
- `reads :: Read a => ReadS a`  `type Reads = String -> [(a, String)]` returns a list of possible parses as (a,String) pairs.
- `maybeRead :: Read a => String -> Maybe a`

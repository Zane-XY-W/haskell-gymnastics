cabal
======


#### cheatsheet
Header One     | Header Two
:------------- | :-------------
build | cabal clean && cabal configure && cabal build
install deps | cabal install --only-dependencies or --only-dep
reinstall a package | ghc-pkg unregister --force persistent-sqlite-1.2.1 <br> cabal install persistent-sqlite-1.2.1
repl inside project | cabal repl


#####quick archetype
cabal actually isn't very strict about how you organize your sources.
```
cabal init
cabal sandbox init
```
now, you can code in `Main.hs` right away.

##### triggered compile
using [hobbes](https://github.com/jhickner/hobbes)

##### [Cabal User guide](http://www.haskell.org/cabal/users-guide/)

#### reduce binary size
By default, GHC links in entire libraries, rather than only the pieces you use; you could avoid this by building libraries with the -split-objs option to GHC (or put split-objs: True in your cabal-install configuration file (~/.cabal/config on Unix)), but it slows down compilation, and is seemingly not recommended by the GHC developers
also see
- [Using shared libraries](http://www.haskell.org/ghc/docs/latest/html/users_guide/using-shared-libs.html)
-

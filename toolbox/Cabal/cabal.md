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

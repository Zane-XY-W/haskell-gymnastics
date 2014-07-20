# List Comprehension

[syntax](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11)

## repeated operations on same value

the boolean guard is more useful than you thought

```
var bonus = 0
if (hasOT) 
  bonus += 100
if (hasLate)
  bonus -= 50
if (isTopPerformer)
  bonus += 500
...

```

```
hasOT = True
hasLate = False
isTopPerformer = True

fs bonus = sum [ f bonus | (p, f) <- [ (hasOT, (+ 100))
                                     , (hasLate, (subtract 50))
                                     , (isTopPerformer, (+ 500))], p]

```
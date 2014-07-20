# Monoid

## conditional appending

`Monoid` can be used to conditional append to a value

In imperative languages,  you could do:

```
input = ""

for seed in ["happy", "coding", "everyday"]:
  if (len(seed) % 2 == 0):
    input += seed [::-1] # reverse str in python
    
print input

```
this patten can be perfectly simulated by using monoidal appending:

```
genStr :: String -> String
genStr input = gen "happy" <> gen "coding" <> gen "everyday"
    where gen seed = if (even $ length seed) then reverse seed ++ input else mempty 

```
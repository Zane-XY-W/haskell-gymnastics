List
======

#### cheatsheet
 purpose     | way
 :------------- | :-------------
if predicate then [elem] else []  | using comprehension: <br> [elem &#124; predicate] <br> [2 &#124; False]  == [] <br> [2 &#124; True] == [2]
comprehension with predicate |  [x \* 2 &#124; x <- [1..10], x \* 2 >= 12]
append single element| [“a”,”b”] ++ [“c”] , “c” could be variable: list ++ [name]
range | `[1 .. 20]` or with a step `[2,5 .. 20]`: the first two element will form up a step <br> reverse range needs a step `[20, 19 .. 1]` <br> infinite `[2,4..]`
existence | ``2 `elem` [1,2,3] ``
indexed access | `[2,4,5] !! 1`
is empty | `null`
head and tail|  head: returns the first element <br> tail: the rest of the list except the first element <br>  last: the last element <br> init: a list without the last element
first n | `take n`
without first n | `drop n`
reverse | `reverse`
ring | `cycle list`
list contains infinite `n` | `repeat n`
list contains `n` `m` | `replicate n m` e.g. `replicated 3 10 == [10, 10, 10]`


#### `:` prepend vs `++` append

- `++`:
  - takes time proportional to the first list length
  - Haskell has to walk through the entire ﬁrst list (the one on the left side of ++), and prepend using cons operator :, so don't using ++ if the first list is large.
- `:`
  - constant time operation. The ++ operator, on the other hand, always takes two lists as arguments
  - Writing `[1,2,3,4] ++ [5]` is wrong.
  - [1,2,3] is just syntactic sugar for `1:2:3:[]`

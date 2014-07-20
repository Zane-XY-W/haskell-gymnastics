# Exceptions

- Exception is not pure, use `reads` if possible. [ref](http://stackoverflow.com/a/5121537/241824)


### read, reads, maybeRead
- read may throw `no parse` error
- `reads :: Read a => ReadS a`  `type Reads = String -> [(a, String)]` returns a list of possible parses as (a,String) pairs.
- `maybeRead :: Read a => String -> Maybe a`



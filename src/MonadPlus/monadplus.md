# MonadPlus
---
## laws
- `mzero` and `mplus` forms a `Monoid`
- `mzero` is a neutral element
    - `mzero mplus m  =  m`
    - `m mplus mzero  =  m`
- `mplus` is associative
-  (not all instances obey this law, because it makes some infinite structures impossible)
m `mplus` (n `mplus` o)  =  (m `mplus` n) `mplus` o

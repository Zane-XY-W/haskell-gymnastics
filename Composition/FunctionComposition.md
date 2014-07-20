# Function Composition


###folding a list of functions

```
fizzBuzzWhizz :: (Int, Int, Int) -> Int -> String
fizzBuzzWhizz (a, b, c) i = case fbw [] of [] -> show i; x -> x
  where fbw = foldl1' (.) $ countOff `map` [(a, []), (a, "Fizz"), (b, "Buzz"), (c, "Whizz")]
        countOff (d, p) r
          | show a `isInfixOf` show i = "Fizz"
          | i `mod` d   == 0          = p ++ r
          | otherwise                 = r
```



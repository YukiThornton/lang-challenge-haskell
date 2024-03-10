
## Operators

### ($): Function Application Operator

```haskell
($) :: (a -> b) -> a -> b

```

```haskell
double :: Int -> Int
double = (*) 2

-- six = six'
six = double $ 3
six' = double 3

-- nine = nine'
nine = (+) 5 $ double 2
nine' = (+) 5 (double 2)


results = zipWith ($) [double, double, double] [1,2,3]
```

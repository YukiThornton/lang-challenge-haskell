# Monad

## Functor
instances: Maybe, IO, []
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
```

```haskell
double :: Int -> Int
double = (*) 2

ghci> fmap double (Just 3)
Just 6
ghci> fmap double Nothing
Nothing
ghci> 5 <$ (Just 3)
Just 5
```

```haskell
--Data.Functor
($>) :: Functor f => f a -> b -> f b
(<$>) :: Functor f => (a -> b) -> f a -> f b -- same as fmap
(<&>) :: Functor f => f a -> (a -> b) -> f b
void :: Functor f => f a -> f ()

ghci> (Just 5) $> 3
Just 3
ghci> double <$> Just 3
Just 6
ghci> Just 3 <&> double
Just 6
ghci> void (Just 3)
Just ()
```

## Monad
instances: Maybe, [], IO, State
```haskell
class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  return :: a -> m a
```

```haskell
doubleMaybe :: Int -> Maybe Int
doubleMaybe x = Just(x * 2)

ghci> Just 3 >>= doubleMaybe 
Just 6
ghci> Just 3 >> Just 2
Just 2
ghci> return 2 :: Maybe Int
Just 2
```

## Do Notation
Syntax fo monads
```haskell
main = do putStr "Hello, "
          putStr "Dog"

main = putStr "Hello, "
       >> putStr "Dog"
```
```haskell
main = do line <- getLine
          putStr line

main = getLine
       >>= putStr
```
```haskell
main = do let x = "Hello, Dog"
          putStr x
main = putStr x
       where x = "Hello, Dog"
```
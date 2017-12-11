## USEFUL HASKELL ##

UsefulFunctions.hs contains functions seen/used in class. Useful for exam.

## Syntax Aid ##

### List Comprehensions: ###

```haskell
list1 = [x1 | x1 <- a_list, ...expressions]
list2 = [(t1,t2,...) | t1 <- list_1, t2 <- list_2, ...expressions]
list3 = [start..end]
list4 = [start..]
```

### Data Types: ###

#### type ####

Create an alias for a certain type to save typing time.
```haskell
type TypeName = OtherType
```
Ex.
```haskell
type State = [(String, [Int])]
```

#### data ####

Create a new data type.
```haskell
data DataType = Constructor1 ...OtherTypes
  | Constructor2 ...OtherTypes
  | ...
```
Ex.
```haskell
data Operation = Add Int Int
  | Sub Int Int
  | ...
```

### Type Classes: ###

#### class ####

Define a class (actually more of an interface), containing a
number of to be defined functions.
```haskell
class TypeClass TypeVars where
  method1 :: SomeType
  ...
  methodN :: SomeType
```
Ex.
```haskell
class Show a where
  show :: a -> String
```

### instance ###

Define an instance of a class.
```haskell
instance TypeClass Type where
  method1 = SomeImplementation
  ...
  methodN = SomeImplementation
```
Ex.
```haskell
instance Show Bool where
  show True  = "True"
  show False = "False"
```

### Functor Class ###

Purpose of a functor is to take a wrapped value and transform the type.
Ex. transforming a Maybe Int to a Maybe Float can be
done by using the fmap function.
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
Instance examples:
```haskell
instance Functor Maybe where
  fmap _ Nothing       = Nothing
  fmap f (Just a)      = Just (f a)
```

### Applicative Class ###

Functor class:
```haskell
class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
Instance examples:
```haskell
instance Applicative Maybe where
  pure = Just  
  Nothing <*> _ = Nothing  
  (Just f) <*> a = fmap f a
```

### Monad Class ###

Monad class.
  return: wraps a given value and returns.
  bind (>>=): takes a wrapped value and function and returns outcome as wrapped value.
```haskell
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```
Instance examples:
```haskell
instance Monad Maybe where
  return :: a -> Maybe a
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
```
```haskell
instance Monad IO where
  return :: a -> IO a
  (>>=) :: IO a -> (a -> IO b) -> IO b
```

## USEFUL HASKELL ##

UsefulFunctions.hs contains functions seen/used in class. Useful for exam.

## Syntax Aid ##

### List Comprehensions: ###

```haskell
list1 = [x1 | x1 <- a_list, ...guards]
list2 = [(t1,t2,...) | t1 <- list_1, t2 <- list_2, ...guards]
list3 = [start..end]
list4 = [start..]
```

### Data Types: ###

#### Type ####

Create an alias for a certain type to save typing time.
```haskell
type TypeName = OtherType
```
Ex.
```haskell
type State = [(String, [Int])]
```

#### Data ####

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

#### Class ####

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

#### Instance ####

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

`fmap`: Allow regular functions to be applied to wrapped values, and return new wrapped values.
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

`<*>`: Allows wrapped functions to be applied to wrapped
values, and return new wrapped values.  
`pure`: Similar to monad `return`.
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

`return`: Allows pure values to be put back into a wrapped value state. Useful for when you're working with generic monads.  
`>>=`: Allows wrapped values to be applied to functions that take
regular values and return the new wrapped values.
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
  return = Just
  m >>= f =
    case m of
      Nothing -> Nothing
      Just x  -> f x
```

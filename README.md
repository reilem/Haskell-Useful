## USEFUL HASKELL - KUL ##

UsefulFunctions.hs contains functions seen/used in class. Useful for exam.

## General Theory ##

#### Lambda-Calculus ####

There are three main calculation rules:

##### 1. &alpha;-conversion (alpha) #####

&lambda;x.E &#8801; &lambda;y.[y/x]E

By using the generic renaming notation: [y/x]E. Notation expresses that we replace every occurence of x in E with y.

(I.E: renaming of parameters)

##### 2. &beta;-reduction (beta) #####

(&lambda;x.E1)E2 &#8801; [E2/x]E1

Using same notation as previous. Replace every occurence of x in E1 with E2.

(I.E: applying a parameter call)

##### 3. &eta;-reduction/conversion (eta) #####

&lambda;x.(Ex) &#8801; E

If x does not occur freely in E we can state that the expression &lambda;x.(Ex) and E would behave in the same way modulo &beta;-reduction and thus are equivalent.

(I.E: simplifying)

#### Structural Recursion ####

When a recursive call is applied to a sub-component of the original data structure it is called Structural Recursion.

Ex. Making a recursive call on the tail of the original list.

It is:
  1. Natural and easy to define.
  2. Well-behaved and guarantees termination.

#### Accumulating parameter ####

Otherwise called an "accumulator". A parameter that is passed along each recursive function call which contains the partial result of all previous calls. When recursion end the accumulator is returned as result, in the case of lists it is often reversed.

Accumulators are used because they allow tail recursion. It prevents any operations from needing to be performed during backtracking after the result is found.

#### Higher Order Functions ####

Functions with **0-th order** take only non-function parameters.

Functions with **n+1-th** order take parameters that are functions of order n.

A function that has a 0-th order function as paramter is a 1st order function.

#### Polymorphism (Parametric / ADT)####

The usage of generic types in function signatures is called Parametric Polymorphism.

Ex.
```haskell
(++) :: [a] -> [a] -> [a]
```
The usage of generic types in Algebraic Data Type (ADTs) are called Polymorphic ADTs.

Ex.
```haskell
data Tree a :: Leaf a | Node (Tree a) (Tree a)
```
If generics are not used then it is called 'Monomorphic'.

#### Currying ####

The concept of Currying is when every function with n parameters is actually simulated by an (n-1)'th order function that takes the first parameter and returns a (n-2)'th order list that processes the remaining characters.

Ex.
```haskell
f :: Num a => a -> a -> a
-- Function f with definition:
f a b = a + b
-- Is the same as:
f a = (+ a)
```

The technique of applying a function to fewer than all of its parameters in order to obtain a different function is called **partial application**.

#### Why Type Classes? ####

Functions are mainly placed into type classes to declare that the function is overloadable.

#### Constrained Polymorphism ####

When a generic parameter is constrained to have specific type class implementation it is called Constrained Polymorphism.

Ex.
```haskell
nub :: Eq a => [a] -> [a]
```

The function is then called a **qualified type**, and `Eq a` is called a **type constraint**.

#### Laziness in Haskell ####

Instead of call-by-value or call-by-name(reference) haskell uses Lazy Evaluation or call-by-need.

If call-by-value and call-by-name evaluation both yield a value then the values will be identical. But they won't always both yield a value.

Ex.
```haskell
loop = loop
call x y = x

-- Call by reference
call 42 loop = 42
-- Call by Value will not terminate
```
Call by reference will sometimes double amount of work needed, call by value is more efficient.

Call-by-need = best of both worlds. Will always terminate and won't double the amount of work. When an expression is evaluated the compiler will replace each occurrence of that expression with the same evaluation.

Advantages:

##### Infinite Data Structures #####

##### Compositionality #####

##### Data Flow Dependancies #####


## Syntax Aid ##

### Operator/function naming ###
An infix operator can be defined using infix notation. It can then be used in either infix or prefix form by surrounding it with brackets.
```haskell
@@ :: Float -> Float -> Float
x @@ y = (x * 2) + (y / 4)

test1 :: Float -> Float -> Float
test1 a b = 5 + (a @@ b)

test2 :: Float -> Float -> Float
test2 a b = 5 + ((@@) a b)
```
A prefix function defined normally can be used in infix form by surrounding it with backticks.
```haskell
f :: Int -> Int -> Int
f a b = a + b + 1

test1 :: Int -> Int -> Int
test1 a b = 5 * (a `f` b)
```

### List Comprehensions: ###

```haskell
-- General form
list = [variables | ...generators, ...guards]
-- General example
list1 = [(t1,t2,...) | t1 <- list_1, t2 <- list_2, predicate(t1), predicate(t2), ...]
-- List between start and end
list2 = [start..end]
-- Infinite list
list3 = [start..]
```

### Anonymous Functions ###

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

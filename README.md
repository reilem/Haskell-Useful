## USEFUL HASKELL ##

UsefulFunctions.hs contains functions seen/used in class. Useful for exam.

## Syntax Aid ##

### Data Types: ###

#### Keyword: type ####

Create an alias for a certain type to save typing time.
```haskell
type Type_name = Other_type
```
Ex.
```haskell
type State = [(String, [Int])]
```

#### Keyword: data ####

Create a new data type.
```haskell
data Data_type_name = Constructor_name_1 ...Other_types
  | Constructor_name_2 ...Other_types
  | ...
```
Ex.
```haskell
data Operation = Add Int Int
  | Sub Int Int
  | ...
```

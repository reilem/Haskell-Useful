-- INTERPRETER

data Exp = Const Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Int
eval (Const a)   = a
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- COMPILER

data Inst = IPush Int | IAdd | ISub | IMul
  deriving Show

type Prog = [Inst]
type Stack = [Int]

execute :: Inst -> Stack -> Stack
execute (IPush a) s    = (a:s)
execute IAdd (x:(y:s)) = ((x+y):s)
execute ISub (x:(y:s)) = ((y-x):s)
execute IMul (x:(y:s)) = ((x*y):s)

run :: Prog -> Stack -> Stack
run (p:[]) s  = execute p s
run (p:ps) s  =
  let nextMem = execute p s in
  run ps nextMem

compile :: Exp -> Prog
compile (Const a) = [IPush a]
compile (Add e1 e2) = (compile e1) ++ (compile e2) ++ [IAdd]
compile (Sub e1 e2) = (compile e1) ++ (compile e2) ++ [ISub]
compile (Mul e1 e2) = (compile e1) ++ (compile e2) ++ [IMul]

-- APPROX. PI

sumf :: [Float] -> Float
sumf l = foldl (+) 0.0 l

productf :: [Float] -> Float
productf l = foldl (*) 1.0 l

piSum :: Float -> Float
piSum n = 8.0 * sumf [ (1.0 / ((1.0 + (4.0 * x)) * (3.0 + (4.0 * x)))) | x <- [0..n]]

piProd :: Float -> Float
piProd n = 4 * productf [(((2 * x) + 2) * ((2 * x) + 4))/(((2 * x) + 3)**2) | x <- [0..n]]

-- PRIME NUMBERS

sieve :: Int -> [Int]
sieve n = performCheck [2..n]
  where
    performCheck :: [Int] -> [Int]
    performCheck []     = []
    performCheck (x:xs) = x : (performCheck $ removeMultiples x xs)

    removeMultiples :: Int -> [Int] -> [Int]
    removeMultiples _ []   = []
    removeMultiples a list = [i | i <- list, i `mod` a /= 0]

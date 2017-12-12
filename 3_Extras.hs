import Control.Monad

-- ## HAMMING ##

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] l = l
merge l [] = l
merge (x:xs) (y:ys)
  | x < y   = x : merge xs (y:ys)
  | x > y   = y : merge (x:xs) ys
  | x == y  = x : merge xs ys

hamming :: [Integer]
hamming = 1 : merge h2 (merge h3 h5)
  where
    h2 = map (2*) hamming
    h3 = map (3*) hamming
    h5 = map (5*) hamming

-- ## PASCAL ##

row :: [Integer] -> [Integer]
row prev = make 0 prev
  where
    make :: Int -> [Integer] -> [Integer]
    make n prev
      | n == 0 = head prev:make 1 prev
      | n == l = [prev !! (l-1)]
      | otherwise = (prev !! n)+(prev !! (n-1)):make (n+1) prev
    l :: Int
    l = length prev

-- modeloplossing
row' :: [Integer] -> [Integer]
row' prev = zipWith (+) (0:prev) (prev++[0])

pascal :: [[Integer]]
pascal = [1]:map row pascal

-- ## BINOMIAL COEFFICIENT ##

bincoeff :: Int -> Int -> Integer
bincoeff n k = (pascal!!n)!!k

-- ## MAY FAIL ##

data MayFail e a = Error e | Result a
  deriving (Eq, Show, Ord)

safeDiv :: Int -> Int -> MayFail String Int
safeDiv a b
  | b == 0    = Error "Division by zero"
  | otherwise = Result (div a b)

instance Functor (MayFail e) where
  fmap f (Error e) = Error e
  fmap f (Result a) = Result (f a)

instance Applicative (MayFail e) where
  pure = return
  (<*>) = ap

instance Monad (MayFail e) where
  return             = Result
  (Error e)  >>= _   = Error e
  (Result x) >>= f   = f x

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp | Div Exp Exp
  deriving (Eq, Ord, Show)

eval :: Exp -> MayFail String Int

eval (Lit i)      = Result i
eval (Div e1 e2)  = eval e1 >>= (\ v1 ->
                      eval e2 >>= safeDiv v1
                    )
eval (Add e1 e2)  = eval e1 >>= (\ v1 ->
                      eval e2 >>= (\ v2 ->
                        Result (v1 + v2)
                      )
                    )
eval (Mul e1 e2)   = eval e1 >>= (\ v1 ->
                      eval e2 >>= (\ v2 ->
                        Result (v1 * v2)
                      )
                    )

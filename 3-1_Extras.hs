-- ## WRITER MONAD ##

import Control.Monad (ap)

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp
  deriving (Show)

evalTrace :: Exp -> (Int, String)
evalTrace (Lit i)     = (i, "Lit\n")
evalTrace (Add e1 e2) =
  let t1 = evalTrace e1 in
  let t2 = evalTrace e2 in
   (fst t1 + fst t2, "Add\n" ++ (snd t1) ++ (snd t2))
evalTrace (Mul e1 e2) =
  let t1 = evalTrace e1 in
  let t2 = evalTrace e2 in
    (fst t1 * fst t2, "Mul\n" ++ (snd t1) ++ (snd t2))

data Writer a = Writer a String
  deriving (Show)

instance Functor Writer where
  fmap f (Writer a s) = Writer (f a) s

instance Applicative Writer where
  pure  = return
  (<*>) = ap

instance Monad Writer where
  return i = Writer i ""
  Writer a _ >>= f = f a

trace :: String -> Writer ()
trace s = Writer () s

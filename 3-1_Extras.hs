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
  Writer a s >>= f =
    let Writer b s' = f a in
      Writer b (s'++s)

trace :: String -> Writer ()
trace s = Writer () s

evalTraceM :: Exp -> Writer Int
evalTraceM (Lit i)      = Writer i "Lit\n"
evalTraceM (Add e1 e2)  = evalTraceM e2 >>= (\ v2 ->
                            evalTraceM e1 >>= (\ v1 ->
                              Writer (v1+v2) "Add\n"
                            )
                          )
evalTraceM (Mul e1 e2)  = evalTraceM e2 >>= (\ v2 ->
                            evalTraceM e1 >>= (\ v1 ->
                              Writer (v1*v2) "Mul\n"
                            )
                          )

-- IMPLEMENTATION MONADIC FUNCTIONS

mySequence :: Monad m => [m a] -> m [a]
mySequence []     = return []
mySequence (m:ms) = m >>= (\ v ->
                            mySequence ms >>= (\ l ->
                              return (v:l)
                            )
                          )

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

-- ## IMPLEMENTATION MONADIC FUNCTIONS ##

mySequence :: Monad m => [m a] -> m [a]
mySequence []     = return []
mySequence (m:ms) = m >>= (\ v ->
                            mySequence ms >>= (\ l ->
                              return (v:l)
                            )
                          )

myMapM :: Monad m => (a -> m b) -> [a] -> m [b]
myMapM f []     = return []
myMapM f (x:xs) =
  let w = f x in
  w >>= (\ v ->
          myMapM f xs >>= (\ l ->
            return (v:l)
          )
        )

myZipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
myZipWithM f []     []     = return []
myZipWithM f (a:as) (b:bs) =
  let mc = f a b in
  mc >>=  (\ c ->
            myZipWithM f as bs >>= (\ l -> return (c:l))
          )

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n x = x >>= (\ v -> return $ replicate n v)

-- ## WRITER AND MYFAIL MONADS ##

data Log e a = LogError e | LogResult a String
  deriving (Show)

instance Functor (Log e) where
  fmap f (LogError e)    = LogError e
  fmap f (LogResult a s) = LogResult (f a) s

instance Applicative (Log e) where
   pure  = return
   (<*>) = ap

instance Monad (Log e) where
  return a              = LogResult a ""
  (LogError e)    >>= _ = LogError e
  (LogResult a s) >>= f =
    case f a of
      (LogError e')    -> LogError e'
      (LogResult b s') -> LogResult b (s'++s)

traceLog :: String -> Log e ()
traceLog s = LogResult () s

safeDivLog :: Int -> Int -> Log String Int
safeDivLog a b =
  if b == 0 then
    LogError "Divide by zero"
  else
    LogResult (a `div` b) "Div\n"

data Exp' = Lit' Int
               | Add' Exp' Exp'
               | Mul' Exp' Exp'
               | Div'  Exp' Exp'
               deriving (Show)

evalLog :: Exp' -> Log String Int
evalLog (Lit' a)     = LogResult a "Lit\n"
evalLog (Add' e1 e2) =
  evalLog e2 >>= (\ v2 ->
  evalLog e1 >>= (\ v1 ->
    LogResult (v1+v2) "Add\n"
  ))
evalLog (Mul' e1 e2) =
  evalLog e2 >>= (\ v2 ->
  evalLog e1 >>= (\ v1 ->
    LogResult (v1*v2) "Add\n"
  ))
evalLog (Div' e1 e2) =
  evalLog e2 >>= (\ v2 ->
  evalLog e1 >>= (\ v1 ->
    safeDivLog v1 v2
  ))

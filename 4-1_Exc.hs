
prog1 :: IO ()
prog1 =
  do m <- readLn
     n <- getLine
     putStrLn "--"
     putStrLn $ unlines $ replicate m n

prog1' :: IO ()
prog1' =
  do m <- getLine
     n <- getLine
     putStrLn "--"
     putStrLn $ unlines $ take (read m) (repeat n)

prog2 :: IO ()
prog2 =
  do
    p <- getLine
    case p of
      "" -> return ()
      s  -> do {putStrLn $ reverse p; prog2}

index :: [IO a] -> IO Int -> IO a
index ioLst ioInt =
  do
    i <- ioInt
    ioLst !! i

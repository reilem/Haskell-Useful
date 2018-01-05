import Control.Monad

-- Pack a list
pack :: Eq a => [a] -> [[a]]
pack list = packup list [] []
  where
    packup :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
    packup []     sub acc =   acc ++ [sub]
    packup (a:as) []  acc =   packup as [a] acc
    packup (a:as) sub acc =
      if a == (head sub) then packup as (a:sub) acc
      else                    packup as [a] (acc++[sub])

-- Big split string
splitString' :: Char -> String -> [String]
splitString' c s = split c s "" []
  where
    split :: Char -> String -> String -> [String] -> [String]
    split c' []     sub acc
      | sub == ""           = reverse acc
      | otherwise           = reverse (sub:acc)
    split c' (c:cs) ""  acc = split c' cs [c]        acc
    split c' (c:cs) sub acc
      | c' == c             = split c' cs ""         (sub:acc)
      | otherwise           = split c' cs (sub++[c]) acc
-- Smaller version
splitString :: Char -> String -> [String]
splitString c' s = reverse $ foldl add [] s
  where
    add :: [String] -> Char -> [String]
    add [] c
      | c == c'   = []
      | otherwise = [[c]]
    add rl@(r:rs) c
      | c == c'   = "":rl
      | otherwise = (r++[c]):rs

-- IO TESTS
runIO :: Int -> IO ()
runIO n = do
    replicateM_ n $ putChar '-'
    putStrLn ""

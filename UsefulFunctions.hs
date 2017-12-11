
replicate :: int -> a -> [a]

words :: String -> [String] -- Breaks up string into words seperated by space

unwords :: [String] -> String -- Join words with seperating space

lines :: String -> [String] -- Breaks up string into lines

unlines :: [String] -> String -- Joins lines with newline

lookup :: Eq a => a -> [(a,b)] -> Maybe b -- Finds value of b corresponding to given a

putStr :: String -> IO () -- Writes string to std. output device

putStrLn :: String -> IO () -- Adds newline character

read :: String -> a

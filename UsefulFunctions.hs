
-- ### LIST SPECIFICS ###

-- Replicate any `a` an int number of times.
replicate :: int -> a -> [a]


-- ### STRING SPECIFICS ###

-- Breaks up string into words seperated by space
words :: String -> [String]

-- Join words with seperating space
unwords :: [String] -> String

-- Breaks up string into lines
lines :: String -> [String]

-- Joins lines with newline
unlines :: [String] -> String


-- ### TUPLE SPECIFICS ###

-- Finds value of b corresponding to given a
lookup :: Eq a => a -> [(a,b)] -> Maybe b

-- Get first element in tuple
fst :: (a,b) -> a

-- Get second element in tuple
snd :: (a,b) -> b

-- Swaps first and second
swap :: (a,b) -> (b,a)


-- #### IO SPECIFICS ####

-- Writes string to std. output device, returns void IO
putStr :: String -> IO ()

-- Adds newline character, returns void IO
putStrLn :: String -> IO ()

-- Takes any showable and prints to std. output device, returns void IO
print :: Show a => a -> IO ()

-- Read line from std. input device, returns IO containing string
getLine :: IO String

-- Takes any showable and converts to String
show :: Show a => a -> String

-- Takes String and convert to any readbale value contained in it
read :: Read a => String -> a


-- ### MONAD SPECIFIC ###

-- (If-without-else) Takes bool condition and performs given monadic if bool succeeds
when :: Monad m => Bool -> m () -> m()
